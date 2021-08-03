import React from "react";
import { useQuery} from "react-query";
import { Switch, Route, Redirect } from "react-router-dom";
import { useServices } from "app/services/services";
import { SiteLayout } from "app/ui/siteLayout/SiteLayout";
import {
  RenderQuery,
  combineQueries,
  officesQueryKey,
  userQueryKey,
} from "app/utils/reactQueryUtils";
import { HomePage } from "app/ui/homePage/HomePage";
import { AdminPage } from "app/ui/adminPage/AdminPage";
import { InfoPage } from "app/ui/infoPage/InfoPage";
import { UserPage } from "app/ui/userPage/UserPage";
import { WelcomePage } from "app/ui/welcomePage/WelcomePage";
import { PlanningPage } from "app/ui/planningPage/PlanningPage";
import { PlaygroundPage } from "app/ui/playgroundPage/PlaygroundPage";
import { CenteredSpinner } from "../ux/spinner";
import { QrOffice } from "app/ui/QrOffice";

export enum RoutePaths {
  Home = "/",
  Welcome = "/welcome",
  Info = "/info",
  QrOffice = "/qr/:office",
  Planning = "/planning",
  User = "/user",
  Admin = "/admin",
  // DEV
  Playground = "/playground",
}

export const AppRoutes: React.FC = () => {
  const { apiClient: apiClientService  } = useServices();

  // Fetch all critical non admin data here
  const userRes = useQuery(userQueryKey(), () => apiClientService.getUser());
  const officesRes = useQuery(officesQueryKey(), () => apiClientService.getOffices());

  return (
    <RenderQuery
      query={combineQueries({
        user: userRes,
        offices: officesRes,
      })}
      onLoading={() => <CenteredSpinner />}
      onError={(error) => <h2>{error.message}</h2>}
    >
      {({ user, offices }) => (
        <SiteLayout user={user}>
          <Switch>
            <Route
              exact
              path={RoutePaths.Welcome}
              render={() => <WelcomePage />}
            />
            <Route exact path={RoutePaths.QrOffice} component={QrOffice} />
            {!user.defaultOffice && <Redirect to={RoutePaths.Welcome} />}

            <Route exact path={RoutePaths.Home} component={HomePage} />
            {user.isAdmin && (
              <Route
                exact
                path={RoutePaths.Admin}
                render={(props) => <AdminPage offices={offices} {...props} />} />
            )}
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
