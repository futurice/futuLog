import { QueryResult } from "react-query";
import {
  IUserDto,
  IShiftAssignmentDto,
  IUserWorkmodeDto,
  IShiftDto,
  IOfficeSpaceDto,
} from "app/services/apiClientService";

// TODO: Utilize this schema in typing `useQuery` calls
export interface IQueryCache {
  user: IUserDto;
  userShift: IShiftAssignmentDto;
  userWorkmodesByDay: Record<string, IUserWorkmodeDto>;
  siteShifts: Record<string, IShiftDto>;
  offices: IOfficeSpaceDto[];
  officeCapacityBySiteDate: Record<string, number>;
}

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
