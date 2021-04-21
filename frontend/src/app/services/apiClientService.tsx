import { stringify as qsStringify } from "query-string";

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
    } else if (res.status === 401) {
        window.location.reload();
    } else {
      return res.text().then(
        (message) => Promise.reject(Error(message)),
        () => Error(`${res.status} ${url}`)
      );
    }
  });
}

const e = encodeURIComponent;

export interface IUserDto {
  email: string;
  name: string;
  picture: string;
  isAdmin: boolean;
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
  site: string;
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

export interface ICapacityDto {
  site?: string;
  date: string;
  people: IUserDto[];
}

export interface IUserBookingsDto {
  userEmail: string;
  site: string;
  date: string;
  workmode: IWorkmodeDto;
}

export interface IAdminUserWorkmodeDto {
  email: string;
  site: string;
  date: string;
  workmode: IWorkmodeDto;
}

export interface IOfficeBookingsRequestDto {
  site: string;
  startDate: string;
  endDate: string;
}

export interface IUserDataRequestDto {
  user: string;
  startDate: string;
  endDate: string;
}

export interface IDeleteUserWorkmodeDto {
  date: string;
  email: string;
}

export function createAPIClientService(baseUrl: string) {
  const apiClient = {
    getUser: () => fetchJSON<IUserDto>(`${baseUrl}/api/me`),

    getUserShift: () => fetchJSON<IShiftAssignmentDto>(`${baseUrl}/api/shift/get`),

    getSiteShifts: (site: string) => fetchJSON<IShiftDto[]>(`${baseUrl}/api/shift/${e(site)}/all`),

    registerSiteShift: (request: ISetShiftDto) =>
      fetchJSON<void>(`${baseUrl}/api/shift/set`, {
        method: "POST",
        body: request,
      }),

    getUserWorkmode: (date: string) =>
      fetchJSON<IUserWorkmodeDto>(`${baseUrl}/api/workmode/get/${e(date)}`),

    getUserWorkmodes: (startDate: string, endDate: string) =>
      fetchJSON<IUserWorkmodeDto[]>(
        `${baseUrl}/api/workmode/batch?${qsStringify({ startDate, endDate })}`
      ),

    registerUserWorkmode: (requests: IRegisterWorkmodeDto[]) =>
      fetchJSON<void>(`${baseUrl}/api/workmode/register`, {
        method: "POST",
        body: requests,
      }),

    confirmUserWorkmode: (confirm: boolean) =>
      fetchJSON<void>(`${baseUrl}/api/workmode/confirm`, {
        method: "POST",
        body: confirm,
      }),

    updateUserWorkmode: (request: IAdminUserWorkmodeDto[]) =>
      fetchJSON<void>(`${baseUrl}/api/admin/workmode/update`, {
        method: "PUT",
        body: request,
      }),

    deleteUserWorkmode: (request: IDeleteUserWorkmodeDto[]) =>
      fetchJSON<void>(`${baseUrl}/api/admin/workmode/remove`, {
        method: "DELETE",
        body: request,
      }),

    getOffices: () => fetchJSON<IOfficeSpaceDto[]>(`${baseUrl}/api/office/all`),

    getUsers: () => fetchJSON<IUserDto[]>(`${baseUrl}/api/admin/people`),

    getOfficeBookings: ({ site, startDate, endDate }: IOfficeBookingsRequestDto) =>
      fetchJSON<ICapacityDto[]>(
        `${baseUrl}/api/office/${e(site)}/booked?${qsStringify({ startDate, endDate })}`
      ),

    getUserContacts: ({ user, startDate, endDate }: IUserDataRequestDto) =>
      fetchJSON<ICapacityDto[]>(
        `${baseUrl}/api/admin/contacts/${e(user)}?${qsStringify({ startDate, endDate })}`
      ),
  };

  return apiClient;
}

export type IAPIClientService = ReturnType<typeof createAPIClientService>;
