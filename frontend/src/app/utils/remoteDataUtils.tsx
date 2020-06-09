import { useSelector, useDispatch } from "react-redux";
import {
  RemoteData,
  IRemoteStore,
  remoteStore,
  getRemoteData,
  getRemoteDataValue,
} from "app/stores/remoteStore";
import { useEffect } from "react";

//
// React hooks

type StateWithRemoteData = { remoteStore: IRemoteStore };

export function useRemoteData<T>(key: keyof IRemoteStore, id: string) {
  const res = useSelector<StateWithRemoteData, RemoteData<T>>((state) =>
    getRemoteData<T>(state.remoteStore, key, id)
  );
  return res;
}

export function useRemoteDataValue<T, K extends keyof IRemoteStore = keyof IRemoteStore>(
  key: K,
  id: string,
  defaultValue: T
): T {
  return useSelector<StateWithRemoteData, T>(
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    (state) => getRemoteDataValue(getRemoteData(state.remoteStore, key, id), defaultValue)
  );
}

const noneSingleton = RemoteData.None();

export function useRemoteDataFetch<T, K extends keyof IRemoteStore = keyof IRemoteStore>(
  key: K,
  id: string | undefined | null | false,
  fetch: (id: string) => Promise<T>,
  // NOTE: Shield the caller from making the mistake of not providing deps,
  // this would lead into infinite loop fetching and setting the data
  deps: unknown[] = []
) {
  const res = useSelector<StateWithRemoteData, RemoteData<T>>(
    (state) => !!id && (state.remoteStore[key] as any)?.[id]
  );
  const dispatch = useDispatch();

  useEffect(() => {
    if (!res && id) {
      dispatch(remoteStore.actions.setLoading({ key, id: id as any }));
      fetch(id as any)
        .then((value) => dispatch(remoteStore.actions.setLoaded({ key, id: id as any, value })))
        .catch((error) => dispatch(remoteStore.actions.setError({ key, id: id as any, error })));
    }
  }, [res, key, id, ...deps]); // eslint-disable-line

  return res || noneSingleton;
}

//
// Render

interface IRenderRemoteData<T> {
  remoteData: RemoteData<T>;
  onNone?: () => React.ReactElement | null;
  onLoading?: () => React.ReactElement | null;
  onError?: (error: Error) => React.ReactElement | null;
  children: (data: T) => React.ReactElement | null;
}

export function RenderRemoteData<T>({
  remoteData,
  onNone,
  onLoading,
  onError,
  children,
}: IRenderRemoteData<T>) {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  return RemoteData.match(remoteData as any, {
    None: () => (onNone ? onNone() : null),
    Loading: () => (onLoading ? onLoading() : null),
    Loaded: (data: T) => children(data),
    Error: (error) => (onError ? onError(error) : null),
  });
}
