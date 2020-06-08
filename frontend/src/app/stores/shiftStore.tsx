import { IServices } from "app/services/services";

export interface IShift {
  days: number[];
  name: string;
  site: string;
}

export const shiftStore = {
  fetchers: {
    fetchSiteShifts: ({ apiClientService }: IServices) => (site: string) =>
      apiClientService.getSiteShifts(site),
  },
};
