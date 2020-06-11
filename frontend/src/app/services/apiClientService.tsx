interface IRequestInit extends RequestInit {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  body?: any;
}

function fetchJSON<T>(url: string, init?: IRequestInit | undefined): Promise<T> {
  return fetch(url, {
    ...init,
    body: init?.body && typeof init.body !== "string" ? JSON.stringify(init.body) : init?.body,
    headers: {
      "Content-Type": "application/json;charset=utf-8",
      ...init?.headers,
    },
  }).then((res) => {
    if (res.ok) {
      // NOTE: Some endpoints return empty result while still reporting
      // Content-Type: application/json, so just revert to null
      // value if the JSON parsing fails
      return res.json().catch(() => null);
    } else {
      throw Error(`${res.status} ${url}`);
    }
  });
}

const e = encodeURIComponent;

export interface IUserDto {
  email: string;
  firstName: string;
  lastName: string;
  avatarUrl: string;
}

export interface IShiftAssignmentDto {
  userEmail: string;
  assignmentDate: string;
  site: string;
  shiftName: string;
}

export interface IShiftDto {
  days: number;
  name: string;
  site: string;
}

export interface ISetShiftDto {
  shiftName: string;
}

export enum Workmode {
  Office = "Office",
  Client = "Client",
  Leave = "Leave",
  Home = "Home",
}

export interface IWorkmodeDto {
  type: Workmode;
  confirmed?: boolean;
  name?: string;
}

export interface IUserWorkmodeDto {
  userEmail: string;
  site: string;
  date: string;
  workmode: IWorkmodeDto;
}

export interface IRegisterWorkmodeDto {
  site: string;
  date: string;
  workmode: IWorkmodeDto;
}

export interface IOfficeSpaceDto {
  site: string;
  maxPeople: number;
}

export function createAPIClientService(baseUrl: string) {
  return {
    getUser: () =>
      // TODO: Fetch /api/me
      Promise.resolve<IUserDto>({
        email: "office-tracker@futurice.com",
        firstName: "Office",
        lastName: "Tracker",
        avatarUrl: "https://placekitten.com/300/300",
      }),

    getUserShift: () => fetchJSON<IShiftAssignmentDto>(`${baseUrl}/api/shift/get`),

    getSiteShifts: (site: string) => fetchJSON<IShiftDto[]>(`${baseUrl}/api/shift/${e(site)}/all`),

    registerSiteShift: (site: string, request: ISetShiftDto) =>
      fetchJSON<void>(`${baseUrl}/api/shift/${e(site)}/set`, {
        method: "POST",
        body: request,
      }),

    getUserWorkmode: (date: string) =>
      fetchJSON<IUserWorkmodeDto>(`${baseUrl}/api/workmode/get/${e(date)}`),

    registerUserWorkmode: (request: IRegisterWorkmodeDto) =>
      fetchJSON<void>(`${baseUrl}/api/workmode/register`, {
        method: "POST",
        body: request,
      }),

    confirmUserWorkmode: (confirm: boolean) =>
      fetchJSON<void>(`${baseUrl}/api/workmode/confirm`, {
        method: "POST",
        body: confirm,
      }),

    // DEV/Admin only
    getWorkmodes: () => fetchJSON<IUserWorkmodeDto[]>(`${baseUrl}/api/workmode/all`),

    getOffices: () => fetchJSON<IOfficeSpaceDto[]>(`${baseUrl}/api/office/all`),

    getOfficeCapacity: (site: string, date: string) =>
      fetchJSON<number>(`${baseUrl}/api/office/${e(site)}/capacity/${e(date)}`),
  };
}

export type IAPIClientService = ReturnType<typeof createAPIClientService>;
