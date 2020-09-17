import React, { useState, useContext } from "react";
import { H4, P } from "../ux/text";
import { Stack, HR, Flex, HorizontalStack } from "../ux/containers";
import { IUserDto } from "app/services/apiClientService";
import { TextField } from "@material-ui/core";
import { Button } from "../ux/buttons";
import { ModalContext } from '../../providers/ModalProvider';
import { colors } from "../ux/theme";
import { Searchbar } from "../ux/searchbox";

interface AddEmployeeModalContent {
  users: IUserDto[],
  site: string,
  date: string,
  onAddEmployee: (userEmail: string, site: string, date: string,) => void
}

//Styling for Add guest / add employee button
// const divider = makeStyles({
//   root: {
//     background: colors["deep-blue-20"],
//     border: `1px solid ${colors["deep-blue-20"]}`,
//     boxSizing: "content-box",
//     display: "inline",
//     margin: "0.75rem 0.5rem",
//   },
// });

// const active = makeStyles({
//   border: {
//     border: `1px solid ${colors["deep-blue-80"]}`,
//   },
// });

const AddEmployeeModalContent: React.FC<AddEmployeeModalContent> = ({ users, onAddEmployee, site, date }) => {
  const { handleModalClose } = useContext(ModalContext);
  const [currentUser, setCurrentUser] = useState({ name: "", email: "" });

  const usersOptions = (users).map(({ first_name, last_name, email }) => ({
    value: email,
    label: `${first_name} ${last_name}`,
  }));

  {/* TODO: Uncomment following lines when addGuest functionality is added */ }
  // const dividerClass = divider();
  // const activeClass = active();

  const [toggleEmployeeAndGuest, setToggleEmployeeAndGuest] = useState(true);
  const showEmployee = toggleEmployeeAndGuest;
  const showGuest = !toggleEmployeeAndGuest;

  {/* TODO: Uncomment following lines when addGuest functionality is added */ }
  // const employeeClass = showEmployee ? activeClass.border : "";
  // const guestClass = showGuest ? activeClass.border : "";

  //Actions
  const handleUserChange = (event: React.ChangeEvent<{}>, value: any | null) => {
    if (value) {
      setCurrentUser({ ...currentUser, name: value.label, email: value.value })
    }
    return null
  }

  {/* TODO: Uncomment following lines when addGuest functionality is added */ }
  // const toggleEmployeeAndGuestState = () => {
  //   setToggleEmployeeAndGuest(!toggleEmployeeAndGuest);
  // };

  return (
    <>
      <H4>Add people to the list</H4>
      <Stack spacing="1.5rem" maxWidth="26rem" mx="auto">
        <P>
          You can add a futurice colleague
          {/* TODO: Uncomment following lines when addGuest functionality is added */}
          {/* You can add a futurice colleague or an external guest. If it’s a new colleague that
          doesn’t have FUM access yet, please add as a guest. */}
        </P>
        <HR />
        {/* TODO: Uncomment following lines when addGuest functionality is added */}
        {/* <Flex>
          <ButtonDiscrete className={employeeClass} 
          onClick={toggleEmployeeAndGuestState}>
            Futurice employee
					</ButtonDiscrete>
          <Box className={dividerClass.root} />
          <ButtonDiscrete className={guestClass} onClick={toggleEmployeeAndGuestState}>
            Guest
					</ButtonDiscrete>
        </Flex> */}
        {showEmployee && (
          <>
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
                onClick={() => onAddEmployee(currentUser.email, date, site)}
              >
                Add
							</Button>
            </HorizontalStack>
          </>
        )}
        {showGuest && (
          <>
            {/* TODO: Show guest input */}
          </>
        )}
      </Stack>
    </>
  );
}

export default AddEmployeeModalContent