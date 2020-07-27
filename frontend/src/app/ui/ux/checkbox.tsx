import React from "react";
import { styled } from "@material-ui/core/styles";
import { colors } from "./theme";
import {Checkbox as CheckboxMui} from "@material-ui/core";
import {CheckboxProps as MuiCheckBoxProps} from "@material-ui/core/Checkbox";

export const Checkbox = styled((props: MuiCheckBoxProps) => <CheckboxMui {...props} disableRipple/>)({
    color: `${colors["deep-blue-80"]}`,
    "&:checked":{
      color: `${colors["deep-blue-80"]}`,
    },
    "&:Mui-checked":{
      color: `${colors["deep-blue-80"]}`,
    },
    "&.MuiCheckbox-colorSecondary.Mui-checked":{
      color:`${colors["deep-blue-80"]}`,
    },
    "&.MuiSvgIcon-root":{
      borderRadius:"4px",
    },

});
