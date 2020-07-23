import React, { useState } from 'react';
import { styled } from '@material-ui/core/styles';
import { Select as MuiSelect } from '@material-ui/core';
import { SelectProps as MuiSelectProps } from '@material-ui/core/Select';
import MenuItem from '@material-ui/core/MenuItem';
import InputLabel from '@material-ui/core/InputLabel';
import FormControl from '@material-ui/core/FormControl';
import { makeStyles } from '@material-ui/core/styles';

import { colors, theme } from './theme';
import { IconArrowUp } from './icons';


export const DropDown = styled((props: MuiSelectProps) => <MuiSelect {...props} />)({
  color: `${colors['deep-blue-80']}`,
  border: `1px solid ${colors['deep-blue-80']}`,
  boxSizing: 'border-box',
  borderRadius: '4px',
  '&:hover': {
    border: '1px solid #6643EF',
    backgroundColor: '#D2CEE3',
  },
  '&:active': {
    backgroundColor: '#D2CEE3',
  },
  '&:disabled': {
    background: 'rgba(255, 255, 255, 0.5)',
    border: '1px solid rgba(166, 157, 199, 0.5)',
    color: 'rgba(255, 255, 255, 0.5)'
  },
  '&.MuiSelect-icon': {
    background: `${colors['deep-blue-80']}`,
    color: `${colors['white']}`,
  },
  list: {
    paddingTop: 0,
    paddingBottom: 0,
    background: 'white',
    '& li': {
      fontWeight: 200,
      paddingTop: 12,
      paddingBottom: 12,
    },
    '& li:hover': {
      background: `${colors['deep-blue-80']}`
    },
    '& li.Mui-selected': {
      color: 'white',
      background: `${colors['deep-blue-50']}`
    },
    '& li.Mui-selected:hover': {
      background: `${colors['deep-blue-80']}`
    }
  }
});


const useStyles = makeStyles({
  formControl: {
    margin: theme.spacing(1),
    minWidth: 120,
    color: `${colors['deep-blue-80']}`,
    border: `1px solid ${colors['deep-blue-80']}`,
    boxSizing: 'border-box',
    borderRadius: '4px',
    '&:hover': {
      border: '1px solid #6643EF',
      backgroundColor: '#D2CEE3',
    },
    '&:active': {
      backgroundColor: '#D2CEE3',
    },
    '&:disabled': {
      background: 'rgba(255, 255, 255, 0.5)',
      border: '1px solid rgba(166, 157, 199, 0.5)',
      color: 'rgba(255, 255, 255, 0.5)'
    },
    '&.MuiSelect-icon': {
      background: `${colors['deep-blue-80']}`,
      color: `${colors['white']}`,
    },
  },
  selectEmpty: {
    marginTop: theme.spacing(2),
  },
  list: {
    paddingTop: 0,
    paddingBottom: 0,
    background: 'white',
    '& li': {
      fontWeight: 200,
      paddingTop: 12,
      paddingBottom: 12,
    },
    '& li:hover': {
      background: `${colors['deep-blue-80']}`
    },
    '& li.Mui-selected': {
      color: 'white',
      background: `${colors['deep-blue-50']}`
    },
    '& li.Mui-selected:hover': {
      background: `${colors['deep-blue-80']}`
    },
  },
  icon:{
    color: `${colors["deep-blue-50"]}`,
    right: 12,
    position: 'absolute',
    userSelect: 'none',
    pointerEvents: 'none'
  },
  button: {
    display: 'block',
    marginTop: theme.spacing(2),
  },
});

// export interface entryData{
//   entryValue: string[];
//   entryText: string[];
// }

export default function Select() { //(entry: entryData, label: string){
  const classes = useStyles()
  // const entryText = entry[0];
  // const entryValue = entry[1];
  // const entryLen = entryText.length;

  // moves the menu below the select input

  const [val, setVal] = React.useState<string | number>('');
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
      vertical: 1,//'bottom',
      horizontal: 1,//'left'
    },
    transformOrigin: {
      vertical: 1, //'top',
      horizontal: 1 //'left'
    },
    getContentAnchorEl: null
  };


  console.log('open:::', open ? 'true' : 'false');

  return (
    <div>
      <FormControl className={classes.formControl}>
        <InputLabel id="demo-controlled-open-select-label">label</InputLabel>
        <DropDown
          labelId="demo-controlled-open-select-label"
          id="demo-controlled-open-select"
          value={val}
          open={open}
          onClose={handleClose}
          onOpen={handleOpen}
          onChange={handleChange}
          IconComponent={() => open ? (<img src="/Icon-arrow-down.png" />) : (<img src="/Icon-arrow-up.png"/>)}


          MenuProps={menuProps}
        >
          <MenuItem value="">
            <em>None</em>
          </MenuItem>
          <MenuItem value={10}>Ten</MenuItem>
          <MenuItem value={20}>Twenty</MenuItem>
          <MenuItem value={30}>Thirty</MenuItem>
        </DropDown>
      </FormControl>
    </div>
  );
}
