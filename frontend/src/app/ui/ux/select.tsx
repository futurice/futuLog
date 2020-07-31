import React from "react";
import { styled } from "@material-ui/core/styles";
import { Select as MuiSelect } from "@material-ui/core";
import { SelectProps as MuiSelectProps } from "@material-ui/core/Select";
import { makeStyles } from "@material-ui/core/styles";

import { colors } from "./theme";
import { IconArrowUp, IconArrowDown } from "./icons";
import { Flex } from "./containers";

type ISelectProps = MuiSelectProps;

export const StyledSelect = styled((props: MuiSelectProps) => <MuiSelect {...props} />)({
  color: `${colors["deep-blue-80"]}`,
  border: "none",
  backgroundColor: colors.white,
  borderRadius: "4px",

  transition: "background-color 250ms cubic-bezier(0.4, 0, 0.2, 1) 0ms," +
    "box-shadow 250ms cubic-bezier(0.4, 0, 0.2, 1) 0ms," +
    "border 250ms cubic-bezier(0.4, 0, 0.2, 1) 0ms",

  "& .MuiInputBase-input": {
    padding: "8px 40px 8px 8px",
    border: `2px solid ${colors["deep-blue-80"]}`,
    borderRadius: "4px",

    transition: "box-shadow 250ms cubic-bezier(0.4, 0, 0.2, 1) 0ms," +
      "border 250ms cubic-bezier(0.4, 0, 0.2, 1) 0ms",
  },

  "& .MuiSelect-icon": {
    width: "36px",
    height: "37px",
    top: "unset",
    right: "0",

    justifyContent: "center",
    alignItems: "center",

    border: `2px solid ${colors["deep-blue-80"]}`,
    backgroundColor: colors["deep-blue-80"],
    borderTopRightRadius: "5px",
    borderBottomRightRadius: "5px",

    transition: "background-color 250ms cubic-bezier(0.4, 0, 0.2, 1) 0ms," +
      "border 250ms cubic-bezier(0.4, 0, 0.2, 1) 0ms",
  },

  "&:hover .MuiInputBase-input": {
    border: `2px solid ${colors["deep-blue-50"]}`,
    boxShadow: "2px 2px 4px rgba(10, 3, 37, 0.2)",
  },

  "&:hover .MuiSelect-icon": {
    backgroundColor: colors["deep-blue-50"],
    border: `2px solid ${colors["deep-blue-50"]}`,
  },

  "&.Mui-focused": {
    "& .MuiInputBase-input": {
      border: `2px solid ${colors["deep-blue-50"]}`,
      boxShadow: "2px 2px 4px rgba(10, 3, 37, 0.2)",
    },

    "& .MuiSelect-icon": {
      border: `2px solid ${colors["deep-blue-50"]}`,
    }
  },

  "&.Mui-disabled": {
    opacity: 0.5,

    "&:hover": {
      "& .MuiInputBase-input": {
        border: `2px solid ${colors["deep-blue-80"]}`,
        boxShadow: "none",
      },
      "& .MuiSelect-icon": {
        backgroundColor: colors["deep-blue-80"],
        border: `2px solid ${colors["deep-blue-80"]}`,
      },
    }
  },

  "& .MuiSelect-root": {
    boxSizing: "border-box",
    width: "200px",
  }
});

const useStyles = makeStyles({
  list: {
    paddingTop: 0,
    paddingBottom: 0,

    backgroundColor: colors.white,
    borderRadius: "4px",
    border: `1px solid ${colors["deep-blue-80"]}`,

    "& option": {
      fontWeight: 200,
      padding: "10px 8px",
      minWidth: "198px",

      transition: "background-color 250ms cubic-bezier(0.4, 0, 0.2, 1) 0ms," +
        "border 250ms cubic-bezier(0.4, 0, 0.2, 1) 0ms",
    },

    "& option:last-child": {
      borderRadius: "0px 0px 4px 4px",
    },
    "& option:first-child": {
      borderRadius: "4px 4px 0px 0px",
    },

    "& option:hover": {
      backgroundColor: colors["deep-blue-20"],
      color: colors["deep-blue-80"],
    },

    "& option:active": {
      fontWeight: "bold",
    },

    "& option:.Mui-selected": {
      color: `${colors["deep-blue-80"]}`,
      backgroundColor: `${colors.white}`,
      fontWeight: "bold",
    },

    "& option.Mui-selected:hover": {
      color: `${colors["deep-blue-80"]}`,
      backgroundColor: `${colors["deep-blue-20"]}`,
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
      vertical: 40,
      horizontal: 0,
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
        <Flex className={"MuiSelect-icon"}>
          <IconArrowUp
            classNames={"MuiSvgIcon-root"}
            color={colors.white}
          />
        </Flex>
        : <Flex className={"MuiSelect-icon"}>
          <IconArrowDown
            classNames={"MuiSvgIcon-root"}
            color={colors.white}
          />
        </Flex>
      }
      MenuProps={menuProps}
      variant="standard"
      {...props}
    >
      {children}
    </StyledSelect>
  );
}
