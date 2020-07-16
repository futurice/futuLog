import React, { useState } from 'react';
import { styled } from "@material-ui/core/styles";
import { colors, Theme, theme } from "./theme";
import Select, {SelectProps as MuiSelectProps} from '@material-ui/core/Select';
import MenuItem from '@material-ui/core/MenuItem';
import MenuProps from '@material-ui/core/MenuItem';
import InputLabel from '@material-ui/core/InputLabel';
import FormControl from '@material-ui/core/FormControl';
import { makeStyles } from '@material-ui/core/styles';
import ExpandMoreIcon from '@material-ui/icons/ExpandMore';


export type SelectProps = MuiSelectProps;
//export type MyMenuProps = MenuProps;

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
    list: {
      paddingTop:0,
      paddingBottom:0,
      background:'white',
    "& li":{
      fontWeight:200,
      paddingTop:12,
      paddingBottom:12,
    },
    "& li:hover":{
      background: `${colors["deep-blue-80"]}`
    },
    "& li.Mui-selected":{
      color:'white',
      background: `${colors["deep-blue-50"]}`
    },
    "& li.Mui-selected:hover":{
      background: `${colors["deep-blue-80"]}`
    }
  }
});


const useStyles = makeStyles({
    formControl: {
      margin: theme.spacing(1),
      minWidth: 120,
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
    },
    selectEmpty: {
      marginTop: theme.spacing(2),
    },
    list: {
      paddingTop:0,
      paddingBottom:0,
      background:'white',
      "& li":{
        fontWeight:200,
        paddingTop:12,
        paddingBottom:12,
      },
      "& li:hover":{
        background: `${colors["deep-blue-80"]}`
      },
      "& li.Mui-selected":{
        color:'white',
        background: `${colors["deep-blue-50"]}`
      },
      "& li.Mui-selected:hover":{
        background: `${colors["deep-blue-80"]}`
      },
    },
    icon:{
      color: `${colors["deep-blue-50"]}`,
      right: 12,
      position: 'absolute',
      userSelect: 'none',
      pointerEvents: 'none'
    },
});

// export interface entryData{
//   entryValue: string[];
//   entryText: string[];
// }

export default function FutuSelect(){ //(entry: entryData, label: string){
  const classes=useStyles()
  const [val, setNewVal] = React.useState('');
  // const entryText = entry[0];
  // const entryValue = entry[1];
  // const entryLen = entryText.length;
  const handleChange = (event:any) => {
    setNewVal(event.target.value)
  };
  // moves the menu below the select input
  const menuProps = {
    classes: {
      list: classes.list
    },
    anchorOrigin: {
      vertical: 1,//"bottom",
        horizontal: 1,//"left"
    },
    transformOrigin: {
      vertical: 1, //"top",
        horizontal: 1 //"left"
    },
    getContentAnchorEl: null
  };
  const iconComponent = (props:any) => {
  return (
    <ExpandMoreIcon className={props.className + " " + classes.icon}/>
  )};


  return (
    <div>
    <FormControl className={classes.formControl}>
      <InputLabel id="demo-simple-select-label">label</InputLabel>
      <Select
        labelId="demo-simple-select-label"
        id="demo-simple-select"
        value={val}
        onChange={handleChange}
        IconComponent={iconComponent}
        MenuProps= {menuProps}
      >
      <MenuItem value="">
            <em>None</em>
          </MenuItem>
          <MenuItem value={10}>Ten</MenuItem>
          <MenuItem value={20}>Twenty</MenuItem>
          <MenuItem value={30}>Thirty</MenuItem>
      </Select>
    </FormControl>
    </div>
  );
}
