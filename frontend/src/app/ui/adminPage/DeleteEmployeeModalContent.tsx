import React from "react";
import { H4, P } from "../ux/text";
import { Stack, Flex } from "../ux/containers";
import { Button } from "@material-ui/core";
import { useContext } from 'react';
import { ModalContext } from '../../providers/ModalProvider';

//Should be able to delete several employees, should it take in an ARRAY of emails?
interface IDeleteUserModalContent {
	onDeleteEmployee: (email: string, date: string) => void;
	email: string;
	date: string;
}

const DeleteUserModalContent: React.FC<IDeleteUserModalContent> = ({ onDeleteEmployee, email, date }) => {
	const { handleModalClose } = useContext(ModalContext);

	//Actions

	return (
		<>
			<H4>Remove selected people</H4>
			<Stack spacing="1.5rem" maxWidth="26rem" mx="auto">
				<P>
					Are you sure you want to remove the selected persons from the list?
        </P>
				<Flex justifyContent="flex-end">
					<Button variant="outlined" color="primary" onClick={handleModalClose}>
						Cancel
          </Button>

					<Button
						variant="contained"
						color="primary"
						onClick={() => onDeleteEmployee(email, date)}
					>
						Remove
          </Button>
				</Flex>
			</Stack>
		</>
	);
}

export default DeleteUserModalContent;