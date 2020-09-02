import React from "react";
import { H4, P } from "../ux/text";
import { Stack, HR, Flex } from "../ux/containers";
import { Searchbox } from "../ux/searchbox"
import { IUserDto } from "app/services/apiClientService";
import { TextField, FormControl, Box, makeStyles, Button } from "@material-ui/core";
import { ButtonDiscrete } from "../ux/buttons";
import { useContext } from 'react';
import { ModalContext } from '../../providers/ModalProvider';


interface AddEmployeeModalContent {
	users: IUserDto[],
	onUserChange?: (event: React.ChangeEvent<{}>, value: any | null) => void;
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

const AddEmployeeModalContent: React.FC<AddEmployeeModalContent> = ({ users, onUserChange }) => {
	const usersOptions = (users || []).map(({ first_name, last_name, email }) => ({
		value: email,
		label: `${first_name} ${last_name}`,
	}));

	const { handleModalOpen, setSelected, setModalState, selected } = useContext(ModalContext);
	console.log(selected)


	const dividerClass = divider();
	const activeClass = active();

	const [addEmployeeState, setAddEmployee] = React.useState(false);
	const [addGuestState, setAddGuest] = React.useState(false);


	const employeeClass = addEmployeeState ? activeClass.border : "";
	const guestClass = addGuestState ? activeClass.border : "";


	function toggleAddGuestPanel() {
		console.log("add guest")
	}
	return (
		<>
			<H4>Add people to the list</H4>
			<Stack spacing="1.5rem" maxWidth="26rem" mx="auto">
				<P>You can add a futurice colleague or an external guest.
				If it’s a new colleague that doesn’t have FUM access yet,
               please add as a guest.</P>

				<HR />
				<Flex>
					<ButtonDiscrete
						className={employeeClass}
						onClick={toggleAddGuestPanel}>
						Futurice employee
                    </ButtonDiscrete>
					<Box className={dividerClass.root} />
					<ButtonDiscrete
						className={guestClass}
						onClick={toggleAddGuestPanel}>
						Guest
                    </ButtonDiscrete>

				</Flex>
				<Flex flexDirection="column" alignItems="flex-start" textAlign="left">
					<b>Name</b>
					<Searchbox
						id="Person-Searchbox"
						options={usersOptions}
						getOptionLabel={(option) => option.label}
						style={{ width: 200 }}
						onChange={onUserChange}
						renderInput={(params) => <TextField {...params} />}
					/>
				</Flex>

				<Flex justifyContent="flex-end" >
					<Button
						variant="outlined"
						color="primary"
						onClick={toggleAddGuestPanel}
					>
						Cancel
                    </Button>

					<Button
						variant="contained"
						color="primary"
						onClick={toggleAddGuestPanel}
					>
						Add
                    </Button>
				</Flex>
			</Stack>
		</>
	)
}

export default AddEmployeeModalContent