import React from "react";
import { styled } from "@material-ui/core";
import { IUserDto } from "app/services/apiClientService";
import {NAVIGATION_BAR_HEIGHT_PX, NavigationBar} from "app/ui/siteLayout/NavigationBar";
import { colors } from "app/ui/ux/theme";

interface ISiteLayout {
  user: IUserDto;
}

const Styles = styled("div")({
  position: "relative",
  fontFamily: "Roboto, sans-serif",
  backgroundColor: colors.white,
  color: colors["deep-blue-80"],
  fontSize: "16px",
});

const NavigationBarWrapper = styled("div")({
  position: "sticky",
  top: 0,
  zIndex: 1,
});

const MainWrapper = styled("main")({
  display: 'flex',
  flexDirection: 'column',
  height: `calc(100% - ${NAVIGATION_BAR_HEIGHT_PX}px)`
});

export const SiteLayout: React.FC<ISiteLayout> = ({ user, children }) => (
  <Styles className="SiteLayout">
    <NavigationBarWrapper>
      <NavigationBar user={user} />
    </NavigationBarWrapper>
    <MainWrapper>{children}</MainWrapper>
  </Styles>
);
