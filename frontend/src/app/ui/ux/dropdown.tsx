import React from 'react';
import { styled } from "@material-ui/core/styles";
import { colors } from "./theme";
import Select, {SelectProps as MuiSelectProps} from '@material-ui/core/Select';
import MenuItem from '@material-ui/core/MenuItem';



export type SelectProps = MuiSelectProps

//const borderSelectClasses = useBorderSelectStyles()

// moves the menu below the select input
const menuProps = {
    classes: {
      //list: borderSelectClasses.list
    },
    anchorOrigin: {
      vertical: "bottom",
        horizontal: "left"
    },
    transformOrigin: {
      vertical: "top",
        horizontal: "left"
    },
    getContentAnchorEl: null
  };

export const DropDown = styled((props: MuiSelectProps) => <Select {...props} />)({
    color: `${colors["deep-blue-80"]}`,
    border: `1px solid ${colors["deep-blue-80"]}`,
    boxSizing: 'border-box',
    borderRadius: '4px',
    "&:hover": {
      border: '1px solid #6643EF',
      backgroundColor: '#D2CEE3',
    },
    "&:active": {
      backgroundColor: '#D2CEE3',
    },
    "&:disabled":{
      background: 'rgba(255, 255, 255, 0.5)',
      border: '1px solid rgba(166, 157, 199, 0.5)',
      color: 'rgba(255, 255, 255, 0.5)'
    },
    "&.MuiSelect-icon":{
      background: `${colors["deep-blue-80"]}`,
      color: `${colors["white"]}`,
    },
});
