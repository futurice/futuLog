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
  },
  paper: {
    backgroundColor: colors.white,
    boxShadow: "20px 20px 40px rgba(20, 7, 75, 0.12)",
    borderRadius: "8px",
    border: 'none',
    padding: '2rem',
    color: colors["deep-blue-80"],
    textAlign: 'center',
    "&:focus": {
      outline: "none"
    }
  },
});

export const StyledModal: React.FC = ({ children }) => {
  const classes = useStyles();
  const { modalOpen, handleModalClose, selected } = useContext(ModalContext);

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
          {selected}
        </div>
      </Fade>
    </Modal>
  );
}
