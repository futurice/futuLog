import React, { useState } from "react";
import {
  Workmode,
  IWorkmodeDto,
  IShiftAssignmentDto,
  IOfficeSpaceDto,
  IUserWorkmodeDto,
  IUserDto,
  IBookedDto
} from "app/services/apiClientService";
import {ButtonDiscrete, ButtonDiscreteWithEndIcon} from "app/ui/ux/buttons"
import List from '@material-ui/core/List';
import ListItem from '@material-ui/core/ListItem';
import ListItemText from '@material-ui/core/ListItemText';
import ListItemAvatar from '@material-ui/core/ListItemAvatar';
import Avatar from '@material-ui/core/Avatar';
import {makeStyles} from '@material-ui/core/styles'

const divider = makeStyles({
  root:{
    background: '#D2CEE3',
    border: '1px solid #D2CEE3',
    boxSizing: 'content-box',
    display: 'inline',
  }
});

const center = makeStyles({
  root:{
    boxSizing: 'content-box',
    marginLeft: 'auto',
    marginRight: 'auto',
    textAlign: 'center',
    display: 'inline',
    flexDirection: 'column',
    alignItems: 'center',
    alignSelf: 'center',
  }
});

const list_holder = makeStyles({
  root:{
    border: '1px solid #200A74',
    boxSizing: 'border-box',
    borderRadius: '4px',
  }
})

export interface entryData{
  entryValue: string[];
  entryText: string[];
}
interface OfficeControllerProps{
  userOffice: IOfficeSpaceDto,
  officeBookings:IBookedDto[]
}

class WhoIsInOfficeClass extends React.Component{
  constructor(){
    super()
    // this being true results in the who is in the office section being shown
    // false means results in the office being able to be selected
    this.state = { whoIsInOfficeToggle: true}
  }

  _toggleOfficeStates = (bool) => {
    this.setState({
      whoIsInOfficeToggle: bool
    });


  }




}


export const OfficeController = ({userOffice, officeBookings} :OfficeControllerProps)=>
{
    const dividerClass= divider();
    const centerClass = center();
    const list_holderClass = list_holder();
    const office_list: string[] = ['Berlin', 'Helsinki', 'London', 'Munich', 'Oslo', 'Stockholm', 'Stuttgart', 'Tampere']

    return (<div><div className={centerClass.root}>
      <ButtonDiscrete onClick={this.state._toggleOfficeStates.bind(null,true)}>Change Office</ButtonDiscrete>
      <div className={dividerClass.root}/>
      <ButtonDiscreteWithEndIcon onClick={this.state._toggleOfficeStates.bind(null,false)}> Who Booked</ButtonDiscreteWithEndIcon>
      {//TODO turn this into its own function that is called here but written above
        if (this.state.whoIsInOfficeToggle === true) {
          <List className={list_holderClass.root}>
          officeBookings.map((users,index) => {
            return (users.users.map((user, index2) => {
              return <ListItem>
                <ListItemAvatar>
                 <Avatar>
                  <img src={user.portrait_badge_url}/>
                 </Avatar>
                </ListItemAvatar>
                <ListItemText primary={user.first_name + user.last_name}/>
              </ListItem>

            }))
          })
          </List>
        }else{
          //TODO
        }
      }
      </div> </div>)
}
