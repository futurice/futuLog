import { createSlice, PayloadAction } from "@reduxjs/toolkit";
import { IWorkmode } from "app/stores/workmodeStore";
import { AsyncAction } from "app/utils/storeUtils";

export interface IUser {
  email: string;
  site: string;
  shiftName?: string;
  workmode?: IWorkmode;
}

export interface IUserShift {
  assignmentDate: string;
  shiftName: string;
  userEmail: string;
  site: string;
}

export interface IUserWorkmode {
  date: string;
  workmode: IWorkmode;
  userEmail: string;
  site: string;
}

export interface IUserStore {
  user?: IUser;
}

export const userStore = createSlice({
  name: "userStore",
  initialState: {} as IUserStore,
  reducers: {
    setUser(state, { payload: user }: PayloadAction<IUser>) {
      state.user = user;
    },

    setWorkmode(state, { payload: workmode }: PayloadAction<IWorkmode>) {
      state.user && (state.user.workmode = workmode);
    },
  },
});

//
// Async actions

export const fetchUserWorkmode = (date: string): AsyncAction => async (
  dispatch,
  getState,
  { apiClientService }
) => {
  //
  // TODO: This is a temporary hack to get the current workmode for the user
  // using the development endpoint that gives all registered workmodes.
  // Actual implementation should use an endpoint that delivers the current
  // workmode for the authorized user, which doesn't exist yet

  const user = getState().userStore.user;
  if (!user) {
    return;
  }

  const workmodes = await apiClientService.getWorkmodes();
  const workmode = workmodes.find(
    (workmode) => workmode.date === date && workmode.userEmail === user.email
  );

  if (workmode) {
    return dispatch(userStore.actions.setWorkmode(workmode.workmode));
  }
};

export const pushUserWorkmode = (
  date: string,
  workmode: IWorkmode
): AsyncAction => async (dispatch, getState, { apiClientService }) => {
  const user = getState().userStore.user;
  if (!user) {
    return;
  }

  dispatch(userStore.actions.setWorkmode(workmode));

  try {
    await apiClientService.registerWorkmode({
      date,
      site: user.site,
      userEmail: user.email,
      workmode,
    });
  } catch (e) {
    // Revert
    dispatch(userStore.actions.setWorkmode(user.workmode!));
  }
};
