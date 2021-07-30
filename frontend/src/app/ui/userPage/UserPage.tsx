import React, { useState } from "react";
import { Box } from "@material-ui/core";
import { useServices } from "app/services/services";
import { IUserDto } from "app/services/apiClientService";
import { PageMargins, Stack, HR, Flex } from "app/ui/ux/containers";
import { H2 } from "app/ui/ux/text";
import { AvatarIcon } from "app/ui/siteLayout/AvatarIcon";
import { userQueryKey, setDefaultOfficeQueryKey } from "app/utils/reactQueryUtils";
import { useMutation } from "react-query";
import { SiteSelector } from "../siteSelector/SiteSector";
import { Button } from "../ux/buttons";

export const UserPage: React.FC = () => {
    const { apiClient, queryCache } = useServices();
    const user = queryCache.getQueryData<IUserDto>(userQueryKey())!;

    const [currentOffice, setCurrentOffice] = useState(user.defaultOffice || "");

    const [setDefaultOffice] = useMutation(
        (request: string) => apiClient.setDefaultOffice(request),
        {
            onSuccess: () => {
                queryCache.refetchQueries(setDefaultOfficeQueryKey());
            },
        }
    );

    const handleSiteChange = (event: React.ChangeEvent<{ name?: string; value: unknown }>) => {
        setCurrentOffice(event.target.value as string);
    };

    const registerUserSite = (_event: React.MouseEvent<HTMLButtonElement, MouseEvent>) => {
        setDefaultOffice(currentOffice);
    };

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
                        <AvatarIcon src={user?.portrait} />
                        <Box fontWeight="bold" padding="1rem">
                            {user?.name}
                        </Box>
                    </Box>
                    <Button href="/logout" variant="outlined" color="primary">
                        Logout
                    </Button>
                </Flex>
                <HR />
                <SiteSelector
                    handleSiteChange={handleSiteChange}
                    registerUserSite={registerUserSite}
                    currentSite={currentOffice}
                    buttonText="Confirm"
                />
            </Stack>
        </PageMargins>
    );
};
