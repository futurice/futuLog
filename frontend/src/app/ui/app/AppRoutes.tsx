import React, { useState } from "react";
import { useQuery } from "react-query";
import { Switch, Route, Redirect } from "react-router-dom";
import { useServices } from "app/services/services";
import { SiteLayout } from "app/ui/siteLayout/SiteLayout";
import {
  RenderQuery,
  combineQueries,
  officesQueryKey,
  userShiftQueryKey,
  userQueryKey,
} from "app/utils/reactQueryUtils";
import { HomePage } from "app/ui/homePage/HomePage";
import { AdminPage } from "app/ui/adminPage/AdminPage";
import { InfoPage } from "app/ui/infoPage/InfoPage";
import { UserPage } from "app/ui/userPage/UserPage";
import { WelcomePage } from "app/ui/welcomePage/WelcomePage";
import { PlanningPage } from "app/ui/planningPage/PlanningPage";
import { PlaygroundPage } from "app/ui/playgroundPage/PlaygroundPage";

export enum RoutePaths {
  Home = "/",
  Welcome = "/welcome",
  Info = "/info",
  Planning = "/planning",
  User = "/user",
  Admin = "/admin",
  // DEV
  Playground = "/playground",
}

export const AppRoutes: React.FC = () => {
  const { apiClient: apiClientService, localStorage: localStorageService } = useServices();
  const [hasVisitedWelcomePage, setHasVisitedWelcomePage] = useState(
    !!localStorageService.getItem("futulog/started")
  );
  const onVisitWelcomePage = () => {
    localStorageService.setItem("futulog/started", "true");
    setHasVisitedWelcomePage(true);
  };

  // Fetch all critical data here
  const userRes = useQuery(userQueryKey(), () => apiClientService.getUser());
  const userShiftRes = useQuery(userShiftQueryKey(), () => apiClientService.getUserShift());
  const officesRes = useQuery(officesQueryKey(), () => apiClientService.getOffices());

  return (
    <RenderQuery
      query={combineQueries({
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
            <Route
              exact
              path={RoutePaths.Welcome}
              render={() => <WelcomePage onMount={onVisitWelcomePage} />}
            />
            {!hasVisitedWelcomePage && <Redirect to={RoutePaths.Welcome} />}

            <Route exact path={RoutePaths.Home} component={HomePage} />
            <Route exact path={RoutePaths.Admin} component={AdminPage} />
            <Route exact path={RoutePaths.Info} component={InfoPage} />
            <Route exact path={RoutePaths.User} component={UserPage} />
            <Route exact path={RoutePaths.Planning} component={PlanningPage} />

            {process.env.NODE_ENV !== "production" && (
              <Route exact path={RoutePaths.Playground} component={PlaygroundPage} />
            )}

            <Route path="*" component={HomePage} />
          </Switch>
        </SiteLayout>
      )}
    </RenderQuery>
  );
};
