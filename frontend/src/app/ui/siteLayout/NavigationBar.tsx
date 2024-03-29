import React from "react";
import { AppBar, styled, Toolbar, IconButton } from "@material-ui/core";
import { IUserDto } from "app/services/apiClientService";
import { colors } from "app/ui/ux/theme";
import { ButtonWithIcon, LinkButton } from "app/ui/ux/buttons";
import { IconInfoBalloon, IconProfile, IconTracking, IconPlanning, IconLogoLight } from "app/ui/ux/icons";
import { Link } from "react-router-dom";
import { RoutePaths } from "app/ui/app/AppRoutes";
import { MediaQuery } from "app/ui/mediaQuery/MediaQuery";
import { AvatarIcon } from "app/ui/siteLayout/AvatarIcon";


interface INavigationBar {
  user: IUserDto;
}

// Due to sticky header behaviour, this is a useful constant to keep around
export const NAVIGATION_BAR_HEIGHT_PX = 64;

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
  }
}));

const ToolbarButtonContainer = styled("div")({
  marginLeft: "auto",
  display: "flex",

  "& button, & .MuiButtonBase-root": {
    "&:hover": {
      backgroundColor: colors["deep-blue-40"]
    },
    "&:focus": {
      boxShadow: `inset 0 0 0px 3px ${colors["deep-blue-50"]}`
    },
    "&:active": {
      boxShadow: `inset 0 0 0px 1px ${colors.white}`,
      backgroundColor: "initial"
    }
  },
  "& button:disabled": {
    color: colors.white,
    opacity: 0.5
  },
  "& .MuiButtonBase-root.Mui-disabled": {
    color: colors.white,
    opacity: 0.5
  }
})

const StyledToolbar = styled(Toolbar)({
  height: `${NAVIGATION_BAR_HEIGHT_PX}px`,
});

const AppTitleLink = styled(Link)({
  textDecoration: "none",
  display: "flex",
  "& > *": { marginRight: "0.6rem" },
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

const UserCredentials = styled("span")({
  margin: 0,
  fontFamily: "Futurice",
  fontSize: "1rem",
  fontWeight: "bold",
  lineHeight: 1.5,
  color: colors.white,
})
const AvatarContainer = styled("div")({
  display: "flex",
  alignItems: "center",
  '& span:first-of-type': {
    marginLeft: "0.75rem",
  }
});

export const NavigationBar: React.FC<INavigationBar> = ({ user }) => (
  <StyledAppBar position="static" className="NavigationBar">
    <StyledToolbar>
      <AppTitleLink to={RoutePaths.Home} aria-label="Home">
        <IconLogoLight height="32" width="28" /> <AppTitle>futuLog</AppTitle>
      </AppTitleLink>

      {/* Mobile controls */}
      <MediaQuery query={(theme) => theme.breakpoints.down("sm")}>
        <ToolbarButtonContainer>
          <LinkButton to={RoutePaths.Planning} component={IconButton} aria-label="Planning">
            <IconPlanning />
          </LinkButton>

          {user.isAdmin && (
            <LinkButton to={RoutePaths.Admin} component={IconButton} aria-label="Tracking">
              <IconTracking />
            </LinkButton>
          )}

          <LinkButton to={RoutePaths.Info} component={IconButton} aria-label="Info">
            <IconInfoBalloon />
          </LinkButton>

          <LinkButton to={RoutePaths.User} component={IconButton} aria-label="Logout">
            <IconProfile />
          </LinkButton>
        </ToolbarButtonContainer>
      </MediaQuery>

      {/* Desktop controls */}
      <MediaQuery query={(theme) => theme.breakpoints.up("md")}>
        <>
          <ToolbarButtonContainer>
            <LinkButton to={RoutePaths.Planning} component={BarButton} startIcon={<IconPlanning />}>
              Planning
          </LinkButton>

            {user.isAdmin && (
              <LinkButton to={RoutePaths.Admin} component={BarButton} startIcon={<IconTracking />}>
                Administration
              </LinkButton>
            )}

            <LinkButton to={RoutePaths.Info} component={BarButton} startIcon={<IconInfoBalloon />}>
              Info
          </LinkButton>

            <LinkButton to={RoutePaths.User} component={BarButton} startIcon={<IconProfile />}>
              Profile
            </LinkButton>
          </ToolbarButtonContainer>
          <AvatarContainer>
            <AvatarIcon src={user.portrait}/>
            <UserCredentials>{user.name}</UserCredentials>
          </AvatarContainer>
        </>
      </MediaQuery>
    </StyledToolbar>
  </StyledAppBar>
);
