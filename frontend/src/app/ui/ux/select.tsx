import React from "react";
import { styled } from "@material-ui/core/styles";
import { Select as MuiSelect } from "@material-ui/core";
import { SelectProps as MuiSelectProps } from "@material-ui/core/Select";
import MenuItem from "@material-ui/core/MenuItem";
import { makeStyles } from "@material-ui/core/styles";
import { colors } from "./theme";
import { IconArrowUp, IconArrowDown } from "./icons";

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
  "&:focus":{
    border: `1px solid ${colors["deep-blue-50"]}`,
  },
  "&:focus-within":{
    border: `1px solid ${colors["deep-blue-50"]}`,
  },
  "&:disabled": {
    background: "rgba(255, 255, 255, 0.5)",
    border: "1px solid rgba(166, 157, 199, 0.5)",
    color: "rgba(255, 255, 255, 0.5)",
    backgroundColor: "rgba(32, 10, 116, 0.5)",
  },
  "& .MuiSelect-selectMenu":{
    backgroundColor: `${colors.white}`,
    borderRadius: "3px 0px 0px 3px",
  },
  "& .MuiSelect-selectMenu:focus":{
      border:`1px solid ${colors["deep-blue-50"]}`
  },
  "& .MuiSelect-icon":{
    top:"unset",
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
    borderRadius:"4px",
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
    "& li:last-child":{
      borderRadius: "0px 0px 3px 3px",
    },
    "& li:first-child":{
      borderRadius: "3px 3px 0px 0px",
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

  return (
        <StyledSelect
          labelId={label}
          id={label}
          value={val}
          open={open}
          onClose={handleClose}
          onOpen={handleOpen}
          onChange={handleChange}
          IconComponent={() => open ? <IconArrowUp color={colors.white}/> : <IconArrowDown color={colors.white}/>}
          MenuProps={menuProps}
          variant="standard"
          {...props}
        >
          {buildMenuItems(entry)}
        </StyledSelect>
  );
}
