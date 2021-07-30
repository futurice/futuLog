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
  defaultOffice: string;
  name: string;
  portrait: string;
  isAdmin: boolean;
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

export interface IRegistrationDto {
  office: string;
  date: string;
  workmode: IWorkmodeDto;
}

export interface IOfficeDto {
  name: string;
  capacity: number;
}

export interface IContactsDto {
  office: string;
  date: string;
  people: IUserDto[];
}

export interface IAdminRegistrationDto {
  email: string;
  office: string;
  date: string;
  workmode: IWorkmodeDto;
}

export interface IRegistrationIdDto {
  email: string;
  date: string;
}

export interface IOfficeBookingsRequestDto {
  office: string;
  startDate?: string;
  endDate?: string;
}

export interface IUserDataRequestDto {
  user: string;
  startDate?: string;
  endDate?: string;
}

export function createAPIClientService(baseUrl: string) {
  const apiClient = {
    getUser: () => fetchJSON<IUserDto>(`${baseUrl}/api/me`),

    setDefaultOffice: (office: string) => fetchJSON<void>(`${baseUrl}/api/me`, {
      method: "PUT",
      body: `"${office}"`
    }),

    getRegistrations: (startDate?: string, endDate?: string) =>
      fetchJSON<IRegistrationDto[]>(
        `${baseUrl}/api/registrations?${qsStringify({ startDate, endDate })}`
      ),

    setRegistrations: (requests: IRegistrationDto[]) =>
      fetchJSON<void>(`${baseUrl}/api/registrations`, {
        method: "PUT",
        body: requests,
      }),

    confirmWorkmode: (date: string) =>
      fetchJSON<void>(`${baseUrl}/api/registrations/confirm`, {
        method: "PUT",
        body: `"${date}"`,
      }),

    getOffices: () => fetchJSON<IOfficeDto[]>(`${baseUrl}/api/offices`),

    getOfficeBookings: ({ office, startDate, endDate }: IOfficeBookingsRequestDto) =>
      fetchJSON<IContactsDto[]>(
        `${baseUrl}/api/offices/${e(office)}/bookings?${qsStringify({ startDate, endDate })}`
      ),

    admin: {
      deleteRegistrations: (requests: IRegistrationIdDto[]) =>
        fetchJSON<void>(`${baseUrl}/api/admin/registrations`, {
          method: "DELETE",
          body: requests,
        }),

      setRegistrations: (requests: IAdminRegistrationDto[]) =>
        fetchJSON<void>(`${baseUrl}/api/admin/registrations`, {
          method: "PUT",
          body: requests,
        }),

      getUsers: () => fetchJSON<IUserDto[]>(`${baseUrl}/api/admin/people`),

      getUserContacts: ({ user, startDate, endDate }: IUserDataRequestDto) =>
        fetchJSON<IContactsDto[]>(
          `${baseUrl}/api/admin/registrations/${e(user)}/contacts?${qsStringify({ startDate, endDate })}`
        ),
    },
  };

  return apiClient;
}

export type IAPIClientService = ReturnType<typeof createAPIClientService>;
