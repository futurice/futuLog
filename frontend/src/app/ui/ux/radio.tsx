import React from 'react';
import { styled } from "@material-ui/core/styles";
import Radio, {RadioProps as MuiRadioProps} from '@material-ui/core/Radio';
import { colors } from "./theme";

export type RadioProps = MuiRadioProps

export const RadioBox = styled((props: MuiRadioProps) => <Radio {...props} disableRipple/>)({
      // color: `${colors["deep-blue-80"]}`,
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
