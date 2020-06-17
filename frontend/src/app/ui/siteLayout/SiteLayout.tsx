import React from "react";
import { IUserDto } from "app/services/apiClientService";
import { NavigationBar } from "app/ui/siteLayout/NavigationBar";

interface ISiteLayout {
  user: IUserDto;
}

export const SiteLayout: React.FC<ISiteLayout> = ({ user, children }) => (
  <div className="SiteLayout">
    <NavigationBar user={user} />
    <main>{children}</main>
  </div>
);
