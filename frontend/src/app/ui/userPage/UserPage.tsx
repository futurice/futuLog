import React, { useState } from "react";
import { Box } from "@material-ui/core";
import { useServices } from "app/services/services";
import { IUserDto, ISetShiftDto, IShiftAssignmentDto, IOfficeSpaceDto } from "app/services/apiClientService";
import { PageMargins, Stack, HR, Flex } from "app/ui/ux/containers";
import { H2 } from "app/ui/ux/text";
import { AvatarIcon } from "app/ui/siteLayout/AvatarIcon";
import { userQueryKey, userShiftQueryKey, officesQueryKey } from "app/utils/reactQueryUtils";
import { useMutation } from "react-query";
import { SiteSelector } from "../siteSelector/SiteSector";
import { Button } from "../ux/buttons";

export const UserPage: React.FC = () => {
  const { apiClient, queryCache, } = useServices();
  const user = queryCache.getQueryData<IUserDto>(userQueryKey());

  // These are pre-loaded in AppRoutes
  const userShift = queryCache.getQueryData<IShiftAssignmentDto>(userShiftQueryKey());
  const [currentSite, setCurrentSite] = useState((userShift && userShift.site) || "");

  const [registerSiteShift] = useMutation(
    (request: ISetShiftDto) => apiClient.registerSiteShift(request),
    {
      onSuccess: () => {
        queryCache.refetchQueries(userShiftQueryKey());
      },
    }
  );

  const handleSiteChange = (event: React.ChangeEvent<{ name?: string; value: unknown }>) => {
    setCurrentSite(event.target.value as string);
  }

  const registerUserSite = (event: React.MouseEvent<HTMLButtonElement, MouseEvent>) => {
    registerSiteShift({ shiftName: "default", site: currentSite })
  }

  return (
    <PageMargins className="UserPage">
      <Stack spacing="2.5rem" maxWidth="25rem" mx="auto" textAlign="center">
        <H2>Personal page</H2>
        <Flex justifyContent="center">
          <Box
            display="flex"
            flexDirection="row"
            justifyContent="center"
            alignItems="center"
            flexWrap="wrap"
          >
            <AvatarIcon src={user?.portrait_thumb_url} />
            <Box fontWeight="bold" padding="1rem">
              {user?.first_name} {user?.last_name}
            </Box>
          </Box>
          <Button href="https://login.futurice.com/?logout=true" variant="outlined" color="primary">
            Logout
         </Button>
        </Flex>
        <HR />
        <SiteSelector
          handleSiteChange={handleSiteChange}
          registerUserSite={registerUserSite}
          currentSite={currentSite}
          buttonText="Confirm"
        />
      </Stack>
    </PageMargins >
  );
};
