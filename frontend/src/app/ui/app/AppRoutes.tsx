import React from "react";
import { Switch, Route } from "react-router-dom";
import { HomePage } from "app/ui/homePage/HomePage";
import { useRemoteDataFetch, RenderRemoteData } from "app/utils/remoteDataUtils";
import { useServices } from "app/services/services";
import { combineRemoteData } from "app/stores/remoteStore";
import { SiteLayout } from "app/ui/siteLayout/SiteLayout";

export enum RoutePaths {
  Home = "/",
  Welcome = "/welcome",
  Info = "/info",
  Planning = "/planning",
  User = "/user",
}

export const AppRoutes: React.FC = () => {
  const { apiClientService } = useServices();

  // TODO: Read a flag from local storage if user has already opened the app before -
  // if not, redirect the user to welcome-page and mark the flag in local storage

  // Fetch all critical data here
  const userRes = useRemoteDataFetch("users", "0", () => apiClientService.getUser());
  const userShiftRes = useRemoteDataFetch("userShifts", "0", () => apiClientService.getUserShift());
  const officesRes = useRemoteDataFetch("offices", "0", () => apiClientService.getOffices());

  return (
    <RenderRemoteData
      remoteData={combineRemoteData({
        user: userRes,
        userShift: userShiftRes,
        offices: officesRes,
      })}
      // TODO: Present initial loading state
      onLoading={() => <h2>Loading user information..</h2>}
      onError={(error) => <h2>{error.message}</h2>}
    >
      {({ user }) => (
        <SiteLayout user={user}>
          <Switch>
            <Route exact path={RoutePaths.Home} component={HomePage} />
            {/* <Route path="*" component={NotFound} /> */}
          </Switch>
        </SiteLayout>
      )}
    </RenderRemoteData>
  );
};
