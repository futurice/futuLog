import React from "react";
import { styled } from "@material-ui/core/styles";
import { colors } from "./theme";
import {Select as SelectMui} from "@material-ui/core";
import {SelectProps as MuiSelectProps} from "@material-ui/core/Select";
import MenuItem from "@material-ui/core/MenuItem";

const menuProps = {
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

export const Select = styled((props: MuiSelectProps) => <SelectMui {...props} />)({
    color: `${colors["deep-blue-80"]}`,
    border: `1px solid ${colors["deep-blue-80"]}`,
    boxSizing: "border-box",
    borderRadius: "4px",
    "&:hover": {
      border: `1px solid ${colors["deep-blue-50"]}`,
      backgroundColor: `${colors["deep-blue-20"]}`,
    },
    "&:active": {
      backgroundColor: `${colors["deep-blue-20"]}`,
    },
    "&:disabled":{
      background: "rgba(255, 255, 255, 0.5)",
      border: "1px solid rgba(166, 157, 199, 0.5)",
      color: "rgba(255, 255, 255, 0.5)"
    },
    "&.MuiSelect-icon":{
      background: `${colors["deep-blue-80"]}`,
      color: `${colors["white"]}`,
    },
});
