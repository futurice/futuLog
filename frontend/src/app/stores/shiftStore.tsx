import { createSlice, PayloadAction, Dispatch } from "@reduxjs/toolkit";
import { IServices } from "app/services/services";

export interface IShift {
  days: number[];
  name: string;
  site: string;
}

export interface IShiftStore {
  shiftsBySite: Record<string, IShift[]>;
}

export const shiftStore = createSlice({
  name: "shiftStore",
  initialState: { shiftsBySite: {} } as IShiftStore,
  reducers: {
    setSiteShifts(
      state,
      { payload }: PayloadAction<{ site: string; shifts: IShift[] }>
    ) {
      state.shiftsBySite[payload.site] = payload.shifts;
    },
  },
});

// Async actions

export const fetchSiteShifts = (site: string) => (
  dispatch: Dispatch,
  _: any,
  { apiClientService }: IServices
) =>
  apiClientService
    .getSiteShifts(site)
    .then((shifts) =>
      dispatch(shiftStore.actions.setSiteShifts({ site, shifts }))
    );
