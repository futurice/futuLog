import { useSelector, useDispatch } from "react-redux";
import { RemoteData, IRemoteStore, remoteStore } from "app/stores/remoteStore";
import { useEffect } from "react";

//
// React hooks

type StateWithRemoteData = { remoteStore: IRemoteStore };

export function useRemoteData<T>(key: keyof IRemoteStore, id: string) {
  const res = useSelector<StateWithRemoteData, RemoteData<T>>(
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    (state) => (state.remoteStore[key] as any)[id]
  );
  return res;
}

export function useRemoteDataValue<T, K extends keyof IRemoteStore = keyof IRemoteStore>(
  key: K,
  id: keyof IRemoteStore[K],
  defaultValue: T
): T {
  const res = useSelector<StateWithRemoteData, RemoteData<T>>(
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    (state) => (state.remoteStore[key] as any)[id]
  );
  return res
    ? // eslint-disable-next-line @typescript-eslint/no-explicit-any
      RemoteData.match<T, T>(res as any, {
        Loading: () => defaultValue,
        Loaded: (value) => value,
        Error: () => defaultValue,
      })
    : defaultValue;
}

const loadingSingleton = RemoteData.Loading();

export function useRemoteDataFetch<T = unknown>(
  key: keyof IRemoteStore,
  id: string | undefined | null | false,
  fetch: (id: string) => Promise<T>,
  deps: unknown[] = []
) {
  const res = useSelector<StateWithRemoteData, RemoteData<T>>(
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    (state) => !!id && (state.remoteStore[key] as any)?.[id]
  );
  const dispatch = useDispatch();

  useEffect(() => {
    if (!res && id) {
      dispatch(remoteStore.actions.setLoading({ key, id }));
      fetch(id)
        .then((value) => dispatch(remoteStore.actions.setLoaded({ key, id, value })))
        .catch((error) => dispatch(remoteStore.actions.setError({ key, id, error })));
    }
  }, [res, key, id, ...deps]); // eslint-disable-line

  return res || loadingSingleton;
}

//
// Render

interface IRenderRemoteData<T> {
  remoteData: RemoteData<T>;
  onLoading?: () => React.ReactElement | null;
  onLoaded: (data: T) => React.ReactElement | null;
  onError?: (error: Error) => React.ReactElement | null;
}

export function RenderRemoteData<T>({
  remoteData,
  onLoading,
  onLoaded,
  onError,
}: IRenderRemoteData<T>) {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  return RemoteData.match(remoteData as any, {
    Loading: () => (onLoading ? onLoading() : null),
    Loaded: (data: T) => onLoaded(data),
    Error: (error) => (onError ? onError(error) : null),
  });
}
