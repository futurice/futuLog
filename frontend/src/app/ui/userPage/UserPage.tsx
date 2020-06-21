import React from "react";
import { PageMargins, Stack } from "app/ui/ux/containers";
import { H2 } from "app/ui/ux/text";
import { Box, Button } from "@material-ui/core";
import { useRemoteDataValue } from "app/utils/remoteDataUtils";
import { IUserDto } from "app/services/apiClientService";
import { AvatarIcon } from "app/ui/siteLayout/AvatarIcon";

export const UserPage: React.FC = () => {
  const user = useRemoteDataValue<IUserDto>("users", "0", null!);

  return (
    <PageMargins className="UserPage">
      <Stack spacing="2.5rem" maxWidth="38rem" mx="auto" textAlign="center">
        <H2>Personal page</H2>

        <Box
          display="flex"
          flexDirection="row"
          mx="auto"
          justifyContent="center"
          alignItems="center"
          flexWrap="wrap"
        >
          <AvatarIcon src={user.portrait_thumb_url} />

          <Box fontWeight="bold" padding="1rem">
            {user.first_name} {user.last_name}
          </Box>
        </Box>

        <Button href="https://login.futurice.com/?logout=true" variant="contained" color="primary">
          Logout
        </Button>
      </Stack>
    </PageMargins>
  );
};
