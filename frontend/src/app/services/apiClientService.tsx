import { stringify as qsStringify } from "query-string";
import { IUserShift, IUserWorkmode } from "app/stores/userStore";
import { IWorkmode } from "app/stores/workmodeStore";
import { IShift } from "app/stores/shiftStore";

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
      return res.json();
    } else {
      throw Error(`${res.status} ${url}`);
    }
  });
}

const e = encodeURIComponent;

interface IRegisterWorkmode {
  userEmail: string;
  site: string;
  date: string;
  workmode: IWorkmode;
}

export function createAPIClientService(baseUrl: string) {
  return {
    getUserShift: (userEmail: string) =>
      fetchJSON<IUserShift>(`${baseUrl}/api/shift/get?${qsStringify({ userEmail })}`),

    getSiteShifts: (site: string) => fetchJSON<IShift[]>(`${baseUrl}/api/shift/${e(site)}/all`),

    registerWorkmode: (request: IRegisterWorkmode) =>
      fetchJSON<void>(`${baseUrl}/api/workmode/register`, {
        method: "POST",
        body: request,
      }),

    getWorkmodes: () => fetchJSON<IUserWorkmode[]>(`${baseUrl}/api/workmode/all`),
  };
}

export type IAPIClientService = ReturnType<typeof createAPIClientService>;
