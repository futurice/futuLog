import React, { useState, useCallback } from "react";
import { H4, P } from "../ux/text";
import { Stack, HR, Flex, HorizontalStack } from "../ux/containers";
import { Searchbox } from "../ux/searchbox"
import { IUserDto } from "app/services/apiClientService";
import { TextField, Box, makeStyles, Button } from "@material-ui/core";
import { ButtonDiscrete } from "../ux/buttons";
import { useContext } from 'react';
import { ModalContext } from '../../providers/ModalProvider';


interface AddEmployeeModalContent {
	users: IUserDto[],
	onAddEmployee: (userEmail: string) => void
}


const divider = makeStyles({
	root: {
		background: "#D2CEE3",
		border: "1px solid #D2CEE3",
		boxSizing: "content-box",
		display: "inline",
		margin: "10px 8px;",
	},
});

const active = makeStyles({
	border: {
		border: "1px solid #200A74",
	},
});

const AddEmployeeModalContent: React.FC<AddEmployeeModalContent> = ({ users, onAddEmployee }) => {
	const { handleModalClose } = useContext(ModalContext);
	const [currentUser, setCurrentUser] = useState({ name: "", email: "" });

	const usersOptions = (users).map(({ first_name, last_name, email }) => ({
		value: email,
		label: `${first_name} ${last_name}`,
	}));

	const dividerClass = divider();
	const activeClass = active();

	const [toggleEmployeeAndGuest, setToggleEmployeeAndGuest] = useState(true);
	const showEmployee = toggleEmployeeAndGuest;
	const showGuest = !toggleEmployeeAndGuest;

	const employeeClass = showEmployee ? activeClass.border : "";
	const guestClass = showGuest ? activeClass.border : "";


	//Actions

	const handleUserChange = (event: React.ChangeEvent<{}>, value: any | null) => {
		if (value) {
			setCurrentUser({ ...currentUser, name: value.label, email: value.value })
			console.log("EMAIL:", value.value, "NAME:", value.label);
		}
		return null
	}

	const toggleEmployeeAndGuestState = () => {
		setToggleEmployeeAndGuest(!toggleEmployeeAndGuest);
	};


	return (
		<>
			<H4>Add people to the list</H4>
			<Stack spacing="1.5rem" maxWidth="26rem" mx="auto">
				<P>
					You can add a futurice colleague or an external guest. If it’s a new colleague that
					doesn’t have FUM access yet, please add as a guest.
        </P>

				<HR />
				<Flex>
					<ButtonDiscrete className={employeeClass} onClick={toggleEmployeeAndGuestState}>
						Futurice employee
          </ButtonDiscrete>
					<Box className={dividerClass.root} />
					<ButtonDiscrete className={guestClass} onClick={toggleEmployeeAndGuestState}>
						Guest
          </ButtonDiscrete>
				</Flex>
				{showEmployee && (
					<>
						<Flex flexDirection="column" alignItems="flex-start" textAlign="left">
							<b>Name</b>
							<Searchbox
								id="Person-Searchbox"
								options={usersOptions}
								getOptionLabel={(option) => option.label}
								style={{ width: 200 }}
								onChange={handleUserChange}
								renderInput={(params) => <TextField {...params} />}
							/>
						</Flex>
						<HorizontalStack
							spacing="0.8rem"
							marginTop="1.25rem"
							justifyContent="flex-end"
						>
							<Button
								variant="contained"
								color="secondary"
								onClick={handleModalClose}
							>
								Cancel
              </Button>

							<Button
								variant="contained"
								color="primary"
								onClick={() => onAddEmployee(currentUser.email)}
							>
								Add
              </Button>
						</HorizontalStack>
					</>
				)}

				{showGuest && (
					<>
						{/* TODO: Viktorija */}
					</>
				)}
			</Stack>
		</>
	);
}

export default AddEmployeeModalContent