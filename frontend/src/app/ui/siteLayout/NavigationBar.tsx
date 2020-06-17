import React from "react";
import { AppBar, styled, Toolbar } from "@material-ui/core";
import { IUserDto } from "app/services/apiClientService";
import { colors } from "app/ui/ux/theme";
import { IconButton } from "app/ui/ux/buttons";
import { IconInfo, IconProfile } from "app/ui/ux/icons";
import { Link } from "react-router-dom";
import { RoutePaths } from "app/ui/app/AppRoutes";

interface INavigationBar {
  user: IUserDto;
}

const StyledAppBar = styled(AppBar)({
  backgroundColor: colors["deep-blue-90"],
  boxShadow: "none",
});

const AppTitleLink = styled(Link)({
  flexGrow: 1,
});

const AppTitle = styled("h1")({
  fontFamily: "Futurice",
  fontSize: "1.25rem",
  lineHeight: 1.5,
  color: colors.white,
});

const BarButton = styled(IconButton)({
  padding: "0.75rem",
  color: colors.white,
  background: "transparent",
});

const AvatarIcon = styled("img")({
  borderRadius: "100%",
  width: "2.5rem",
  height: "2.5rem",
});

const avatarIconPlaceholder =
  "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8vwkAArYBs3xK2c8AAAAASUVORK5CYII=";

export const NavigationBar: React.FC<INavigationBar> = ({ user }) => (
  <StyledAppBar position="static" className="NavigationBar">
    <Toolbar>
      <AppTitleLink to={RoutePaths.Home}>
        <AppTitle>FutuLog</AppTitle>
      </AppTitleLink>

      <Link to={RoutePaths.Info}>
        <BarButton startIcon={<IconInfo />}>Info</BarButton>
      </Link>

      <BarButton startIcon={<IconProfile />} href="https://login.futurice.com/?logout=true">
        Logout
      </BarButton>

      <Link to={RoutePaths.User}>
        <BarButton
          startIcon={
            <AvatarIcon
              src={user.portrait_thumb_url || avatarIconPlaceholder}
              width="40"
              height="40"
              aria-hidden={true}
            />
          }
        >
          {user.first_name} {user.last_name}
        </BarButton>
      </Link>
    </Toolbar>
  </StyledAppBar>
);
