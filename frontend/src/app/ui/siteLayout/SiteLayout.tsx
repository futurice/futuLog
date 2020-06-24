import React from "react";
import { styled } from "@material-ui/core";
import { IUserDto } from "app/services/apiClientService";
import { NavigationBar } from "app/ui/siteLayout/NavigationBar";
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

export const SiteLayout: React.FC<ISiteLayout> = ({ user, children }) => (
  <Styles className="SiteLayout">
    <NavigationBarWrapper>
      <NavigationBar user={user} />
    </NavigationBarWrapper>
    <main>{children}</main>
  </Styles>
);
