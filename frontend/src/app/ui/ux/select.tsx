import React from "react";
import { styled } from "@material-ui/core/styles";
import { Select as MuiSelect } from "@material-ui/core";
import { SelectProps as MuiSelectProps } from "@material-ui/core/Select";
import { makeStyles } from "@material-ui/core/styles";

import { colors } from "./theme";
import { IconArrowUp, IconArrowDown } from "./icons";

type ISelectProps = MuiSelectProps;

export const StyledSelect = styled((props: MuiSelectProps) => <MuiSelect {...props} />)({
  color: `${colors["deep-blue-80"]}`,
  border: `1px solid ${colors["deep-blue-80"]}`,
  backgroundColor: `${colors["deep-blue-80"]}`,
  borderRadius: "4px",
  "&:hover": {
    border: `1px solid ${colors["deep-blue-50"]}`,
    backgroundColor: `${colors["deep-blue-50"]}`,
    boxShadow: "2px 2px 4px rgba(10, 3, 37, 0.2)",
  },
  "&:active": {
    backgroundColor: `${colors["deep-blue-50"]}`,
    border: `1px solid ${colors["deep-blue-50"]}`,
    boxShadow: "2px 2px 4px rgba(10, 3, 37, 0.2)",
  },
  "&:active-within": {
    backgroundColor: `${colors["deep-blue-50"]}`,
    border: `1px solid ${colors["deep-blue-50"]}`,
  },
  "&:focus": {
    border: `1px solid ${colors["deep-blue-50"]}`,
  },
  "&:focus-within": {
    border: `1px solid ${colors["deep-blue-50"]}`,
  },
  "&:disabled": {
    background: "rgba(255, 255, 255, 0.5)",
    border: "1px solid rgba(166, 157, 199, 0.5)",
    color: "rgba(255, 255, 255, 0.5)",
    backgroundColor: "rgba(32, 10, 116, 0.5)",
  },
  "& .MuiSelect-selectMenu": {
    backgroundColor: `${colors.white}`,
    borderRadius: "3px 0px 0px 3px",
  },
  "& .MuiSelect-selectMenu:focus": {
    border: `1px solid ${colors["deep-blue-50"]}`
  },
  "& .MuiSelect-icon": {
    top: "unset",
  },
  "&:hover .SVGpath":{
    fill: `${colors["deep-blue-50"]}`,
  },
  "& .MuiSelect-root":{
    minWidth:"150px",
  }
});

const useStyles = makeStyles({
  list: {
    paddingTop: 0,
    paddingBottom: 0,
    background: `${colors.white}`,
    borderRadius: "4px",
    border: `1px solid ${colors["deep-blue-80"]}`,
    "& li": {
      fontWeight: 200,
      paddingTop: 12,
      paddingBottom: 12,
      borderRadius: "3px",
      minWidth: 148,
    },
    "& li:hover": {
      color: `${colors["deep-blue-80"]}`,
      background: `${colors["deep-blue-20"]}`,
    },
    "& li.Mui-selected": {
      color: `${colors["deep-blue-80"]}`,
      background: `${colors.white}`,
      fontWeight: "bold",
    },
    "& li.Mui-selected:hover": {
      color: `${colors["deep-blue-80"]}`,
      background: `${colors["deep-blue-20"]}`,
    },
    "& li:last-child": {
      borderRadius: "0px 0px 3px 3px",
    },
    "& li:first-child": {
      borderRadius: "3px 3px 0px 0px",
    },
  },
});


export const Select: React.FC<ISelectProps> = ({
  children,
  ...props
}) => {
  const classes = useStyles()
  const [open, setOpen] = React.useState(false);

  const menuProps = {
    classes: {
      list: classes.list
    },
    anchorOrigin: {
      vertical: 34,
      horizontal: -1,
    },
    transformOrigin: {
      vertical: 0,
      horizontal: 0,
    },
    getContentAnchorEl: null,
    border: `1px solid ${colors["deep-blue-80"]}`,
  };

  const handleClose = () => {
    setOpen(false);
  };

  const handleOpen = () => {
    setOpen(true);
  };

  return (
    <StyledSelect
      open={open}
      onClose={handleClose}
      onOpen={handleOpen}
      IconComponent={() => open ?
        <IconArrowUp
          classNames={"MuiSvgIcon-root MuiSelect-icon"}
          color={colors.white}
        />
        : <IconArrowDown
          classNames={"MuiSvgIcon-root MuiSelect-icon"}
          color={colors.white}
        />
      }
      MenuProps={menuProps}
      variant="standard"
      {...props}
    >
      {children}
    </StyledSelect>
  );
}
