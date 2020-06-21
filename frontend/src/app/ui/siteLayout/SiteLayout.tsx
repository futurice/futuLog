import React from "react";
import { styled } from "@material-ui/core";
import { IUserDto } from "app/services/apiClientService";
import { NavigationBar } from "app/ui/siteLayout/NavigationBar";
import { colors } from "app/ui/ux/theme";

interface ISiteLayout {
  user: IUserDto;
}

const Styles = styled("div")({
  fontFamily: "Roboto, sans-serif",
  backgroundColor: colors.white,
  color: colors["deep-blue-80"],
  fontSize: "16px",
});

export const SiteLayout: React.FC<ISiteLayout> = ({ user, children }) => (
  <Styles className="SiteLayout">
    <NavigationBar user={user} />
    <main>{children}</main>
  </Styles>
);
