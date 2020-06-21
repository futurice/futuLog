import React from "react";
import { Switch, Route } from "react-router-dom";
import { useRemoteDataFetch, RenderRemoteData } from "app/utils/remoteDataUtils";
import { useServices } from "app/services/services";
import { combineRemoteData } from "app/stores/remoteStore";
import { SiteLayout } from "app/ui/siteLayout/SiteLayout";
import { HomePage } from "app/ui/homePage/HomePage";
import { PlaygroundPage } from "app/ui/playgroundPage/PlaygroundPage";
import { InfoPage } from "app/ui/infoPage/InfoPage";
import { UserPage } from "app/ui/userPage/UserPage";

export enum RoutePaths {
  Home = "/",
  Welcome = "/welcome",
  Info = "/info",
  Planning = "/planning",
  User = "/user",
  // DEV
  Playground = "/playground",
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
            <Route exact path={RoutePaths.Info} component={InfoPage} />
            <Route exact path={RoutePaths.User} component={UserPage} />

            {process.env.NODE_ENV !== "production" && (
              <Route exact path={RoutePaths.Playground} component={PlaygroundPage} />
            )}

            {/* <Route path="*" component={NotFound} /> */}
          </Switch>
        </SiteLayout>
      )}
    </RenderRemoteData>
  );
};
