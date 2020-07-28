import React from "react";
import { styled } from "@material-ui/core/styles";
import { Select as MuiSelect } from "@material-ui/core";
import { SelectProps as MuiSelectProps } from "@material-ui/core/Select";
import MenuItem from "@material-ui/core/MenuItem";
import { makeStyles } from "@material-ui/core/styles";
import { colors} from "./theme";
import { IconArrowUp, IconArrowDown } from "./icons";

export const StyledSelect = styled((props: MuiSelectProps) => <MuiSelect {...props} />)({
  color: `${colors["deep-blue-80"]}`,
  border: `1px solid ${colors["deep-blue-80"]}`,
  boxSizing: "border-box",
  backgroundColor: `${colors["deep-blue-80"]}`,
  "&:hover": {
    border: `1px solid ${colors["deep-blue-50"]}`,
    backgroundColor: `${colors["deep-blue-50"]}`,
  },
  "&:active": {
    backgroundColor: `${colors["deep-blue-50"]}`,
    border: `2px solid ${colors["deep-blue-50"]}`,
  },
  "&:disabled": {
    background: "rgba(255, 255, 255, 0.5)",
    border: "1px solid rgba(166, 157, 199, 0.5)",
    color: "rgba(255, 255, 255, 0.5)",
    backgroundColor: "rgba(32, 10, 116, 0.5)",
  },
  "&.MuiSelect-icon": {
    background: `${colors["deep-blue-80"]}`,
    color: `${colors.white}`,
  },
  list: {
    paddingTop: 0,
    paddingBottom: 0,
    background: `${colors.white}`,
    "& li": {
      fontWeight: 200,
      paddingTop: 12,
      paddingBottom: 12,
    },
    "& li:hover": {
      background: `${colors["deep-blue-80"]}`
    },
    "& li:active": {
      background: `${colors["deep-blue-50"]}`
    },
    "& li.Mui-selected": {
      color: `${colors.white}`,
      background: `${colors["deep-blue-50"]}`
    },
    "& li.Mui-selected:hover": {
      background: `${colors["deep-blue-80"]}`
    }
  },
  icon: {
    background: `${colors["deep-blue-80"]}`,
  },
  "& .MuiSelect-selectMenu":{
    backgroundColor: `${colors.white}`,
  }
});

const useStyles = makeStyles({
  list: {
    paddingTop: 0,
    paddingBottom: 0,
    background: `${colors.white}`,
    borderRadius:"4px",
    border: `1px solid ${colors["deep-blue-80"]}`,
    "& li": {
      fontWeight: 200,
      paddingTop: 12,
      paddingBottom: 12,
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
      background: `${colors["deep-blue-80"]}`,
    },
  },
});

export interface entryData{
  value: string;
  text: string;
}

function buildMenuItems(entry:entryData[]){
  return(
    entry.map(({value, text}) =>
      (<MenuItem value={value}>{text}</MenuItem>)
    )
  )
}

interface ISelectProps{
  entry: entryData[],
  label: string
}

export const Select: React.FC<ISelectProps & MuiSelectProps> = ({
  entry,
  label,
  ...props
 }) => {
  const classes = useStyles()
  const [val, setVal] = React.useState<string | number>("");
  const [open, setOpen] = React.useState(false);
  const handleChange = (event: React.ChangeEvent<{ value: unknown }>) => {
    setVal(event.target.value as number);
  };
  const handleClose = () => {
    setOpen(false);
  };
  const handleOpen = () => {
    setOpen(true);
  };
  const menuProps = {
    classes: {
      list: classes.list
    },
    anchorOrigin: {
      vertical: 35,
      horizontal: 1,
    },
    transformOrigin: {
      vertical: 1,
      horizontal: 1,
    },
    getContentAnchorEl: null,
    border: `1px solid ${colors["deep-blue-80"]}`,
  };

  return (
        <StyledSelect
          labelId={label}
          id={label}
          value={val}
          open={open}
          onClose={handleClose}
          onOpen={handleOpen}
          onChange={handleChange}
          IconComponent={() => open ? (<IconArrowDown color={colors.white}/>) : (<IconArrowUp color={colors.white}/>)}
          MenuProps={menuProps}
          {...props}
        >
          {buildMenuItems(entry)}
        </StyledSelect>
  );
}
