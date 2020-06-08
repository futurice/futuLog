import { Union, of } from "ts-union";
import { createSlice, PayloadAction } from "@reduxjs/toolkit";
import { IUserWorkmode, IUser, IUserShift } from "app/stores/userStore";
import { IShift } from "app/stores/shiftStore";

//
// Remote data store shape.
// Used to type access to store via utility functions
// as well as for documentation purposes.

export interface IRemoteStore {
  users: Singleton<IUser>;
  userShifts: Singleton<IUserShift>;
  userWorkmodesByDay: Collection<IUserWorkmode>;
  siteShifts: Collection<IShift>;
}

export type Singleton<T> = { "0"?: RemoteData<T> };
export type Collection<T> = Record<string, RemoteData<T>>;

//
// Store code

export const RemoteData = Union((t) => ({
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

export function getRemoteData<T, K extends keyof IRemoteStore = keyof IRemoteStore>(
  remoteStore: IRemoteStore,
  key: K,
  id: keyof IRemoteStore[K],
  defaultValue: T
): T {
  /* eslint-disable @typescript-eslint/no-explicit-any */
  const dict = remoteStore[key] as Record<keyof IRemoteStore[K], any>;
  return dict[id]
    ? RemoteData.match<T, T>(dict[id], {
        Loading: () => defaultValue,
        Loaded: (value) => value,
        Error: () => defaultValue,
      })
    : defaultValue;
}
