import React from "react";
import { H4, P } from "../ux/text";
import { Stack, HR } from "../ux/containers";
import { Searchbox } from "../ux/searchbox"
import { IUserDto } from "app/services/apiClientService";
import { TextField, FormControl } from "@material-ui/core";
import { Toolbar, ToolbarItem } from "./styled";


interface AddEmployeeModalContent {
    users: IUserDto[],
    onUserChange?: (event: React.ChangeEvent<{}>, value: any | null) => void;

}
const AddEmployeeModalContent: React.FC<AddEmployeeModalContent> = ({ users, onUserChange }) => {
    const usersOptions = (users || []).map(({ first_name, last_name, email }) => ({
        value: email,
        label: `${first_name} ${last_name}`,
    }));
    return (
        <>
            <H4>Add people to the list</H4>
            <Stack spacing="2.5rem" maxWidth="26rem" mx="auto">
                <P>You can add a futurice colleague or an external guest.
                If it’s a new colleague that doesn’t have FUM access yet,
               please add as a guest.</P>

                <HR />

                <ToolbarItem>
                    <b>Name</b>
                    <FormControl>
                        {/* Should receive a prop to change icon to search */}
                        <Searchbox
                            id="Person-Searchbox"
                            options={usersOptions}
                            getOptionLabel={(option) => option.label}
                            style={{ width: 200 }}
                            onChange={onUserChange}
                            renderInput={(params) => <TextField {...params} />}
                        />
                    </FormControl>
                </ToolbarItem>

            </Stack>
        </>
    )
}

export default AddEmployeeModalContent