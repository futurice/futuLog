import React from "react";
import { styled } from "@material-ui/core/styles";
import { Radio as RadioMui } from "@material-ui/core";
import { RadioProps as MuiRadioProps } from "@material-ui/core/Radio";
import { colors } from "./theme";

export const Radio = styled((props: MuiRadioProps) => <RadioMui {...props} disableRipple/>)({
    '&:checked':{
      color: `${colors["deep-blue-80"]}`,
    },
    '&:Mui-checked':{
      color: `${colors["deep-blue-80"]}`,
    },
    '&.MuiRadio-colorSecondary.Mui-checked':{
      color:`${colors["deep-blue-80"]}`,
    },
    '&.MuiIconButton-colorSecondary':{
      color:`${colors["deep-blue-80"]}`,
    },
});
