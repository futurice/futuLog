import React, { createContext, SetStateAction } from "react";
import { useModal } from "../hooks/useModal";

const ModalContext = createContext({
    modalOpen: false,
    handleModalClose: () => { },
    handleModalOpen: () => { },
    setModalState: (state: boolean) => { },
    selected: null,
    setSelected: (state: null) => { },
})


const ModalProvider: React.FC = ({ children }) => {
    const { modalOpen, handleModalClose, handleModalOpen, setModalState, selected, setSelected } = useModal();

    return (
        <ModalContext.Provider value={{
            modalOpen,
            handleModalClose,
            handleModalOpen,
            setModalState,
            selected,
            setSelected,
        }}>
            {children}
        </ModalContext.Provider>
    );
}

export { ModalContext, ModalProvider };