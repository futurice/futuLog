import React from "react";
import { H4, P } from "../ux/text";
import { Stack, HorizontalStack } from "../ux/containers";
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
						onClick={() => onDeleteEmployee(email, date)}
					>
						Remove
          </Button>
				</HorizontalStack>
			</Stack>
		</>
	);
}

export default DeleteUserModalContent;