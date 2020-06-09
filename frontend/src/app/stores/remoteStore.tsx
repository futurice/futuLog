import { Union, of } from "ts-union";
import { createSlice, PayloadAction } from "@reduxjs/toolkit";
import {
  IUserDto,
  IShiftAssignmentDto,
  IUserWorkmodeDto,
  IShiftDto,
} from "app/services/apiClientService";

//
// Remote data store shape.
// Used to type access to store via utility functions
// as well as for documentation purposes.

export interface IRemoteStore {
  users: Singleton<IUserDto>;
  userShifts: Singleton<IShiftAssignmentDto>;
  userWorkmodesByDay: Collection<IUserWorkmodeDto>;
  siteShifts: Collection<IShiftDto>;
}

export type Singleton<T> = { "0"?: RemoteData<T> };
export type Collection<T> = Record<string, RemoteData<T>>;

//
// Store code

export const RemoteData = Union((t) => ({
  // NOTE: None is used as a sentinel on utility functions.
  // None should _not_ be inserted into the store
  None: of<void>(),
  Loading: of<void>(),
  Loaded: of(t),
  Error: of<Error>(),
}));

// Declare a dummy type for RemoteData for typing purposes
export interface RemoteData<T = unknown> {}

export const remoteStore = createSlice({
  name: "remoteStore",
  initialState: {
    users: {},
    userShifts: {},
    userWorkmodesByDay: {},
    siteShifts: {},
  } as IRemoteStore,
  reducers: {
    setLoading(
      state,
      { payload: { key, id } }: PayloadAction<{ key: keyof IRemoteStore; id: string }>
    ) {
      /* eslint-disable @typescript-eslint/no-explicit-any */
      state[key] = (state[key] || {}) as any;
      (state[key] as any)[id] = RemoteData.Loading();
      /* eslint-enable @typescript-eslint/no-explicit-any */
    },
    setLoaded(
      state,
      {
        payload: { key, id, value },
      }: PayloadAction<{ key: keyof IRemoteStore; id: string; value: unknown }>
    ) {
      /* eslint-disable @typescript-eslint/no-explicit-any */
      state[key] = (state[key] || {}) as any;
      (state[key] as any)[id] = RemoteData.Loaded(value);
      /* eslint-enable @typescript-eslint/no-explicit-any */
    },
    setError(
      state,
      {
        payload: { key, id, error },
      }: PayloadAction<{ key: keyof IRemoteStore; id: string; error: Error }>
    ) {
      /* eslint-disable @typescript-eslint/no-explicit-any */
      state[key] = (state[key] || {}) as any;
      (state[key] as any)[id] = RemoteData.Error(error);
      /* eslint-enable @typescript-eslint/no-explicit-any */
    },
  },
});

const noneSingleton = RemoteData.None();

export function getRemoteData<T, K extends keyof IRemoteStore = keyof IRemoteStore>(
  remoteStore: IRemoteStore,
  key: K,
  id: string
): IRemoteStore[K][] {
  /* eslint-disable @typescript-eslint/no-explicit-any */
  const dict = remoteStore[key] as Record<keyof IRemoteStore[K], any>;
  return (dict as any)[id] || noneSingleton;
}

export function getRemoteDataValue<T>(remoteData: RemoteData<T>, defaultValue: T) {
  return remoteData
    ? RemoteData.match<T, T>(remoteData as any, {
        None: () => defaultValue,
        Loading: () => defaultValue,
        Loaded: (value) => value,
        Error: () => defaultValue,
      })
    : defaultValue;
}

export function mapRemoteData<T, U>(fn: (value: T) => U, remoteData: RemoteData<T>): RemoteData<U> {
  return RemoteData.match<RemoteData<U>, T>(remoteData as any, {
    None: () => remoteData,
    Loading: () => remoteData,
    Loaded: (value) => RemoteData.Loaded(fn(value)),
    Error: () => remoteData,
  });
}

// Due to constraints of typing, all input values have to have the same type
export function mapRemoteDataMany<T, U>(
  fn: (...values: T[]) => U,
  ...remoteDatas: Array<RemoteData<T>>
): RemoteData<U> {
  if (remoteDatas.some((remoteData) => RemoteData.if.Loading(remoteData as any, () => true))) {
    return RemoteData.Loading();
  }

  const error = remoteDatas.find((remoteData) =>
    RemoteData.if.Error(remoteData as any, () => true)
  );
  if (error) {
    return error;
  }

  if (remoteDatas.some((remoteData) => RemoteData.if.None(remoteData as any, () => true))) {
    return RemoteData.None();
  }

  const values = remoteDatas.map((remoteData) =>
    RemoteData.if.Loaded(remoteData as any, (value) => value)
  );

  return RemoteData.Loaded(fn(...(values as any)));
}

export function combineRemoteData<M>(
  remoteDataMap: { [K in keyof M]: RemoteData<M[K]> }
): RemoteData<M> {
  const remoteDatas = Object.values(remoteDataMap) as RemoteData[];

  if (remoteDatas.some((remoteData) => RemoteData.if.Loading(remoteData as any, () => true))) {
    return RemoteData.Loading();
  }

  const error = remoteDatas.find((remoteData) =>
    RemoteData.if.Error(remoteData as any, () => true)
  );
  if (error) {
    return error;
  }

  if (remoteDatas.some((remoteData) => RemoteData.if.None(remoteData as any, () => true))) {
    return RemoteData.None();
  }

  return RemoteData.Loaded(
    Object.keys(remoteDataMap).reduce((memo, next) => {
      memo[next] = RemoteData.if.Loaded((remoteDataMap as any)[next], (value) => value);
      return memo;
    }, {} as any)
  );
}
