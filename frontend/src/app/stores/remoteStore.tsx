import { Union, of } from "ts-union";
import { createSlice, PayloadAction, Dictionary } from "@reduxjs/toolkit";
import { IUserWorkmode, IUser, IUserShift } from "app/stores/userStore";
import { IShift } from "app/stores/shiftStore";

//
// Remote data store shape.
// For documentation purposes
// TODO: Consider always using [key, id] pair to access remote data items

export interface IRemoteStore {
  users: Singleton<RemoteData<IUser>>;
  userShifts: Singleton<RemoteData<IUserShift>>;
  userWorkmodesByDay: Dictionary<RemoteData<IUserWorkmode>>;
  siteShifts: Dictionary<RemoteData<IShift>>;
}

type Singleton<T> = { "0"?: RemoteData<T> };

//
// Store code

export const RemoteData = Union((t) => ({
  Loading: of<void>(),
  Loaded: of(t),
  Error: of<Error>(),
}));

// Declare a dummy type for RemoteData for typing purposes
export interface RemoteData<T = any> {}

export const remoteStore = createSlice({
  name: "remoteStore",
  initialState: {} as IRemoteStore,
  reducers: {
    setLoading(
      state,
      {
        payload: { key, id },
      }: PayloadAction<{ key: keyof IRemoteStore; id: string }>
    ) {
      state[key] = state[key] || {};
      (state[key] as any)[id] = RemoteData.Loading();
    },
    setLoaded(
      state,
      {
        payload: { key, id, value },
      }: PayloadAction<{ key: keyof IRemoteStore; id: string; value: any }>
    ) {
      state[key] = state[key] || {};
      (state[key] as any)[id] = RemoteData.Loaded(value);
    },
    setError(
      state,
      {
        payload: { key, id, error },
      }: PayloadAction<{ key: keyof IRemoteStore; id: string; error: Error }>
    ) {
      state[key] = state[key] || {};
      (state[key] as any)[id] = RemoteData.Error(error);
    },
  },
});
