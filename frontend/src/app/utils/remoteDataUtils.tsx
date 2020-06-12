import { useSelector, useDispatch } from "react-redux";
import {
  RemoteData,
  IRemoteStore,
  remoteStore,
  getRemoteData,
  getRemoteDataValue,
} from "app/stores/remoteStore";
import { useEffect } from "react";
import { AsyncAction } from "app/utils/storeUtils";

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
      dispatch(remoteStore.actions.setLoading({ key, id }));
      fetch(id as any)
        .then((value) => dispatch(remoteStore.actions.setLoaded({ key, id, value })))
        .catch((error) => dispatch(remoteStore.actions.setError({ key, id, error })));
    }
  }, [res, key, id, ...deps]); // eslint-disable-line

  return res || noneSingleton;
}

export function pushRemoteData<T>(
  key: keyof IRemoteStore,
  id: string,
  push: () => Promise<T>
): AsyncAction {
  return async (dispatch) => {
    dispatch(remoteStore.actions.setLoading({ key, id }));
    push()
      .then((value) => dispatch(remoteStore.actions.setLoaded({ key, id, value })))
      .catch((error) => dispatch(remoteStore.actions.setError({ key, id, error })));
  };
}

//
// Render

interface IRenderRemoteData<T, U> {
  remoteData: RemoteData<T>;
  onNone?: () => React.ReactElement | null;
  onLoading?: (
    data: T | undefined,
    children: (data: T, ...extra: U[]) => React.ReactElement | null
  ) => React.ReactElement | null;
  onError?: (
    error: Error,
    children: (data: T, ...extra: U[]) => React.ReactElement | null
  ) => React.ReactElement | null;
  children: (data: T, ...extra: U[]) => React.ReactElement | null;
}

export function RenderRemoteData<T, U = any>({
  remoteData,
  onNone,
  onLoading,
  onError,
  children,
}: IRenderRemoteData<T, U>) {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  return RemoteData.match(remoteData as any, {
    None: () => (onNone ? onNone() : null),
    Loading: (data?: T) => (onLoading ? onLoading(data, children) : null),
    Loaded: (data: T) => children(data),
    Error: (error) => (onError ? onError(error, children) : null),
  });
}
