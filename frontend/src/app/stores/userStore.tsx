import { IWorkmode } from "app/stores/workmodeStore";
import { AsyncAction } from "app/utils/storeUtils";
import { getRemoteData, remoteStore } from "app/stores/remoteStore";
import { IServices } from "app/services/services";

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

export const userStore = {
  fetchers: {
    fetchUserWorkmode: ({ apiClientService, storeService }: IServices) => async (date: string) => {
      const state = storeService.getState();

      //
      // TODO: This is a temporary hack to get the current workmode for the user
      // using the development endpoint that gives all registered workmodes.
      // Actual implementation should use an endpoint that delivers the current
      // workmode for the authorized user, which doesn't exist yet

      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      const user = getRemoteData<IUser>(state.remoteStore, "users", "0", null!);
      if (!user) {
        throw Error("fetchUserWorkmode: NOT LOGGED IN");
      }

      const workmodes = await apiClientService.getWorkmodes();
      const workmode = workmodes.find(
        (workmode) => workmode.date === date && workmode.userEmail === user.email
      );

      if (workmode) {
        return workmode;
      }

      throw Error("fetchUserWorkmode: Not found");
    },

    fetchUserShift: ({ apiClientService, storeService }: IServices) => async () => {
      const state = storeService.getState();

      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      const user = getRemoteData<IUser>(state.remoteStore, "users", "0", null!);
      if (!user) {
        throw Error("fetchUserShift: NOT LOGGED IN");
      }

      const userShift = await apiClientService.getUserShift(user.email);
      return userShift;
    },
  },
  actions: {
    fakeLogin: (user: IUser): AsyncAction => async (dispatch) => {
      dispatch(remoteStore.actions.setLoaded({ key: "users", id: "0", value: user }));
    },
    pushUserWorkmode: (date: string, workmode: IWorkmode): AsyncAction => async (
      dispatch,
      getState,
      { apiClientService }
    ) => {
      const user = getRemoteData<IUser>(
        getState().remoteStore,
        "users",
        "0",
        null! // eslint-disable-line @typescript-eslint/no-non-null-assertion
      );
      if (!user) {
        throw Error("pushUserWorkmode: NOT LOGGED IN");
      }

      dispatch(remoteStore.actions.setLoading({ key: "userWorkmodesByDay", id: date }));

      try {
        await apiClientService.registerWorkmode({
          date,
          site: user.site,
          userEmail: user.email,
          workmode,
        });
        dispatch(
          remoteStore.actions.setLoaded({
            key: "userWorkmodesByDay",
            id: date,
            value: workmode,
          })
        );
      } catch (error) {
        dispatch(
          remoteStore.actions.setError({
            key: "userWorkmodesByDay",
            id: date,
            error,
          })
        );
      }
    },
  },
};
