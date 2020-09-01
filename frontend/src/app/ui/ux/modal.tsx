import React from "react";
import { makeStyles } from '@material-ui/core/styles';
import { Modal } from '@material-ui/core';
import Backdrop from '@material-ui/core/Backdrop';
import Fade from '@material-ui/core/Fade';

import { colors } from "./theme";


import { useContext } from 'react';
import { ModalContext } from '../../providers/ModalProvider';

const useStyles = makeStyles({
    modal: {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        border: 'none',
    },
    paper: {
        backgroundColor: colors.white,
        borderRadius: "4px",
        border: 'none',
        padding: '32px',
        color: colors["deep-blue-80"],
        textAlign: 'center',
    },
});

export const StyledModal: React.FC = ({ children }) => {
    const classes = useStyles();

    const { modalOpen, handleModalClose } = useContext(ModalContext);

    return (
        <Modal
            aria-labelledby="transition-modal-title"
            aria-describedby="transition-modal-description"
            className={classes.modal}
            open={modalOpen}
            onClose={handleModalClose}
            closeAfterTransition
            BackdropComponent={Backdrop}
            BackdropProps={{
                timeout: 500,
            }}
        >
            <Fade in={modalOpen}>
                <div
                    className={classes.paper}
                >
                    {children}
                </div>
            </Fade>
        </Modal>
    );
}
