import React from "react";
import { styled } from "@material-ui/core/styles";
import { colors } from "./theme";
import Checkbox, {CheckboxProps as MuiCheckBoxProps} from '@material-ui/core/Checkbox';

export type FutuCheckProps = MuiCheckBoxProps

export const FutuCheck = styled((props: MuiCheckBoxProps) => <Checkbox {...props} disableRipple/>)({
    color: `${colors["deep-blue-80"]}`,
    // border: `1px solid ${colors["deep-blue-80"]}`,
    // boxSizing: 'border-box',
    // borderRadius: '4px',

    '&:checked':{
      color: `${colors["deep-blue-80"]}`,
    },
    '&:Mui-checked':{
      color: `${colors["deep-blue-80"]}`,
    },
    '&.MuiCheckbox-colorSecondary.Mui-checked':{
      color:`${colors["deep-blue-80"]}`,
    },
    '&.MuiSvgIcon-root':{
      borderRadius:'4px'
    },
  
});
