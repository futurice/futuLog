import React, { Component } from "react";
import "./tracking.scss";
import "../../App.scss";
import {
  Card,
  CardContent,
  ButtonGroup,
  Button,
  Typography,
} from "@material-ui/core";

class Tracking extends Component {
  render() {
    return (
      <div>
        <Typography
          color="textSecondary"
          variant="h4"
          gutterBottom
          component="h2"
        >
          Trackings
        </Typography>

        <Card>
          <CardContent className="cardContent">
            <Typography color="textSecondary" component="h5" variant="h5">
              Where are you working today?
            </Typography>
            <ButtonGroup
              color="primary"
              aria-label="outlined primary button group"
            >
              <Button>Home</Button>
              <Button>Office</Button>
              <Button>Leave</Button>
            </ButtonGroup>
          </CardContent>
        </Card>
      </div>
    );
  }
}

export default Tracking;
