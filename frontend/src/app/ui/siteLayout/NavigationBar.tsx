import React from "react";
import { AppBar, styled, Toolbar, IconButton } from "@material-ui/core";
import { IUserDto } from "app/services/apiClientService";
import { colors } from "app/ui/ux/theme";
import { ButtonWithIcon } from "app/ui/ux/buttons";
import { IconInfo, IconProfile } from "app/ui/ux/icons";
import { Link } from "react-router-dom";
import { RoutePaths } from "app/ui/app/AppRoutes";
import { MediaQuery } from "app/ui/mediaQuery/MediaQuery";

interface INavigationBar {
  user: IUserDto;
}

const StyledAppBar = styled(AppBar)(({ theme }) => ({
  backgroundColor: colors["deep-blue-90"],
  boxShadow: "none",
  [theme.breakpoints.down("xs")]: {
    "& > .MuiToolBar-root": {
      padding: "0 1.25rem",
    },
  },
  [theme.breakpoints.up("md")]: {
    "& > .MuiToolBar-root": {
      padding: "0.5rem 1.75rem",
    },
  },
}));

const AppTitleLink = styled(Link)({
  flexGrow: 1,
});

const AppTitle = styled("h1")({
  margin: 0,
  fontFamily: "Futurice",
  fontSize: "1.25rem",
  lineHeight: 1.5,
  color: colors.white,
});

const BarButton = styled(ButtonWithIcon)({
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
      <AppTitleLink to={RoutePaths.Home} aria-label="Home">
        <AppTitle>FutuLog</AppTitle>
      </AppTitleLink>

      <MediaQuery query={(theme) => theme.breakpoints.down("xs")}>
        <>
          <Link to={RoutePaths.Info} aria-label="Info">
            <IconButton aria-label="Info">
              <IconInfo />
            </IconButton>
          </Link>

          <Link to={RoutePaths.User} aria-label="Info">
            <IconButton aria-label="Profile">
              <IconProfile />
            </IconButton>
          </Link>
        </>
      </MediaQuery>

      <MediaQuery query={(theme) => theme.breakpoints.up("sm")}>
        <>
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
        </>
      </MediaQuery>
    </Toolbar>
  </StyledAppBar>
);
