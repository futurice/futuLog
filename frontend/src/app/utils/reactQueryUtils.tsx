import { QueryResult, AnyQueryKey } from "react-query";

//
// TODO: Consider typed helpers for `useQuery` and `queryCache.getQuery*`

// IUserDto
export const userQueryKey = () => "user";
// IShiftAssignmentDto
export const userShiftQueryKey = () => "userShift";
// IUserWorkmodeDto
export const userWorkmodeQueryKey = (date: string): AnyQueryKey => ["userWorkmodes", date];
// IUserWorkmodeDto[]
export const userWorkmodesQueryKey = (startDate: string, endDate: string): AnyQueryKey => [
  "userWorkmodes",
  startDate,
  endDate,
];
// IShiftDto
export const siteShiftQueryKey = (): AnyQueryKey => ["siteShifts"];
// export const siteShiftQueryKey = (site: string): AnyQueryKey => ["siteShifts", site];
// IOfficeSpaceDto[]
export const officesQueryKey = () => "offices";
// IUserDto[]
export const usersQueryKey = () => "users";
// number
export const officeCapacityQueryKey = (site: string, date: string): AnyQueryKey => [
  "officeCapacity",
  site,
  date,
];
// ICapacityDto
export const officeBookingsQueryKey = (
  site: string,
  startDate: string,
  endDate: string
): AnyQueryKey => ["officeBookings", site, startDate, endDate];

export const userContactsQueryKey = (
  user: string,
  startDate: string,
  endDate: string
): AnyQueryKey => ["userContacts", user, startDate, endDate];

export const userBookingsQueryKey = (
  user: string,
  startDate: string,
  endDate: string
): AnyQueryKey => ["userBookings", user, startDate, endDate];

export function combineQueries<M>(queryMap: { [K in keyof M]: QueryResult<M[K]> }): QueryResult<M> {
  const queries = Object.values(queryMap) as QueryResult<any>[];

  const loading = queries.find((query) => query.status === "loading");
  if (loading) {
    return loading;
  }

  const error = queries.find((query) => query.status === "error");
  if (error) {
    return error;
  }

  return {
    status: "success",
    data: Object.keys(queryMap).reduce((memo, next) => {
      memo[next] = queryMap[next as keyof typeof queryMap].data;
      return memo;
    }, {} as any),
    error: null,
    isFetching: false,
    isStale: false,
    failureCount: 0,
    refetch: () => Promise.reject("TODO"),
  };
}

interface IRenderQuery<T, U> {
  query: QueryResult<T>;
  onNone?: () => React.ReactElement | null;
  onLoading?: (
    data: T | undefined,
    children: (data: T, ...extra: U[]) => React.ReactElement | null
  ) => React.ReactElement | null;
  onError?: (
    error: Error,
    children: (data: T, ...extra: U[]) => React.ReactElement | null
  ) => React.ReactElement | null;
  children: (data: T, ...extra: U[]) => React.ReactElement | null;
}

export function RenderQuery<T, U = any>({
  query,
  onLoading,
  onError,
  children,
}: IRenderQuery<T, U>) {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  if (query.status === "loading") {
    return onLoading ? onLoading(query.data, children) : null;
  } else if (query.status === "error") {
    return onError ? onError(query.error, children) : null;
  } else {
    return children(query.data);
  }
}
