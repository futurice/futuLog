import { useState } from "react";

export const useModal = (initialMode = false, initialSelected = null) => {
    const [modalOpen, setModalOpen] = useState(initialMode);
    const [selected, setSelected] = useState<any>(initialSelected)


    const handleModalClose = () => {
        setModalOpen(false);
    };

    const handleModalOpen = () =>
        setModalOpen(true);

    const setModalState = (state: boolean) => {
        setModalOpen(state)
        if (state === false) {
            setSelected(null)
        }
    }


    return { modalOpen, handleModalOpen, handleModalClose, setModalState, selected, setSelected };
};