import React from "react";
import { H4, P } from "../ux/text";
import { Stack } from "../ux/containers";

const DeleteUserModalContent: React.FC = () => {

    return (
        <>
            <H4>Remove selected people</H4>
            <Stack spacing="2.5rem" maxWidth="26rem" mx="auto">
                <P>You can add a futurice colleague or an external guest.
                If it’s a new colleague that doesn’t have FUM access yet,
               please add as a guest.</P>
            </Stack>
        </>
    )
}

export default DeleteUserModalContent