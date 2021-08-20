import React, { useState, useContext } from "react";
import { H4, P } from "../ux/text";
import { Stack, HR, Flex, HorizontalStack } from "../ux/containers";
import { IUserDto } from "app/services/apiClientService";
import { TextField } from "@material-ui/core";
import { Button } from "../ux/buttons";
import { ModalContext } from '../../providers/ModalProvider';
import { colors } from "../ux/theme";
import { Searchbar } from "../ux/searchbox";

interface AddAdminModalContent {
  users: IUserDto[],
  onAdd: (userEmail: string) => void
}

const AddAdminModalContent: React.FC<AddAdminModalContent> = ({ users, onAdd }) => {
  const { handleModalClose } = useContext(ModalContext);
  const [currentUser, setCurrentUser] = useState({ name: "", email: "" });

  const usersOptions = (users).map(({ name, email }) => ({
    value: email,
    label: name,
  }));

  //Actions
  const handleUserChange = (event: React.ChangeEvent<{}>, value: any | null) => {
    if (value) {
      setCurrentUser({ ...currentUser, name: value.label, email: value.value })
    }
    return null
  }

  return (
    <>
      <H4>Add an administrator</H4>
      <Stack spacing="1.5rem" maxWidth="26rem" mx="auto">
        <P>
          You can add a futurice colleague
        </P>
        <HR />
          <Flex flexDirection="column" alignItems="flex-start" textAlign="left">
            <b>Name</b>
            <Searchbar
              id="Employee-Searchbox"
              options={usersOptions}
              getOptionLabel={(option) => option.label}
              onChange={handleUserChange}
              style={{
                border: `1px solid ${colors["deep-blue-80"]}`,
                height: "40px",
                borderRadius: "4px",
                marginTop: "6px",
              }}
              renderInput={(params) =>
                <TextField {...params}
                  InputProps={{
                    ...params.InputProps,
                    disableUnderline: true
                  }}
                />
              }
            />
          </Flex>
          <HorizontalStack
            spacing="0.8rem"
            marginTop="1.25rem"
            justifyContent="flex-end"
          >
            <Button
              variant="outlined"
              color="primary"
              onClick={handleModalClose}
            >
              Cancel
            </Button>
            <Button
              variant="contained"
              color="primary"
              onClick={() => onAdd(currentUser.email)}
            >
              Add
            </Button>
          </HorizontalStack>
      </Stack>
    </>
  );
}

export default AddAdminModalContent
