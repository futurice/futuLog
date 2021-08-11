import React, { useState, useContext } from "react";
import { TextField } from "@material-ui/core";
import { Button } from "../ux/buttons";
import { ModalContext } from '../../providers/ModalProvider';
import { H4 } from "../ux/text";
import { Stack, Flex, HorizontalStack } from "../ux/containers";

const AddOfficeModalContent: React.FC<{ onAdd: (name: string, capacity: number) => void }> = ({ onAdd }) => {
  const { handleModalClose } = useContext(ModalContext);
  const [currentOffice, setCurrentOffice] = useState({ name: "", capacity: "0" });

  return (
    <>
      <H4>Add an office to the list</H4>
      <Stack spacing="1.5rem" maxWidth="26rem" mx="auto">
          <Flex flexDirection="column" alignItems="flex-start" textAlign="left">
            <b>Name</b>
            <TextField value={currentOffice.name} onInput={(ev: any) => setCurrentOffice({ ...currentOffice, name: ev.target.value })} />
            <b style={{ marginTop: "1.25rem" }}>Capacity</b>
            <TextField type="number" value={currentOffice.capacity} onInput={(ev: any) => setCurrentOffice({ ...currentOffice, capacity: ev.target.value })} />
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
              onClick={() => {
                onAdd(currentOffice.name, parseInt(currentOffice.capacity))
                handleModalClose();
                }}
            >
              Add
            </Button>
          </HorizontalStack>
      </Stack>
    </>
  );
}

export default AddOfficeModalContent
