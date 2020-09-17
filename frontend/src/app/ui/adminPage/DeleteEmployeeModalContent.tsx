import React, { useContext } from "react";
import { H4, P } from "../ux/text";
import { Stack, HorizontalStack } from "../ux/containers";
import { Button } from "../ux/buttons";
import { ModalContext } from '../../providers/ModalProvider';

interface IDeleteUserModalContent {
  onDeleteEmployee: (date: string) => void;
  date: string;
}

const DeleteUserModalContent: React.FC<IDeleteUserModalContent> = ({ onDeleteEmployee, date }) => {
  const { handleModalClose } = useContext(ModalContext);

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
            variant="outlined"
            color="primary"
            onClick={handleModalClose}
          >
            Cancel
          </Button>

          <Button
            variant="contained"
            color="primary"
            onClick={() => onDeleteEmployee(date)}
          >
            Remove
          </Button>
        </HorizontalStack>
      </Stack>
    </>
  );
}

export default DeleteUserModalContent;