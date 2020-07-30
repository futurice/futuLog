import React from "react";

interface IIcon {
  color?: string;
  backgroundColor?: string;
}

export const IconHome: React.FC<IIcon> = ({ color = "#ff465a" }) => (
  <svg fill="none" height="24" viewBox="0 0 24 24" width="24" xmlns="http://www.w3.org/2000/svg">
    <path
      d="m21.5547 8.05424v-4.52882c0-2.603386-2.0226-3.52542-3.9245-3.52542h-11.29058c-1.90188 0-3.92453.922034-3.92453 3.52542v4.52882c-1.38867.32542-2.41509 1.43729-2.41509 2.79326v7.2678c0 2.1423 1.6 2.983 3.16981 3.0644v1.844c0 .5424.48302.9763 1.08679.9763h1.11698c.60378 0 1.0868-.4339 1.0868-.9763v-1.844h11.07922v1.844c0 .5424.483.9763 1.0868.9763h1.117c.6038 0 1.0868-.4339 1.0868-.9763v-1.844c2.3245-.1085 3.1698-1.6272 3.1698-3.0644v-7.295c-.0604-1.32877-1.0868-2.46776-2.4453-2.76606zm-18.05281-4.52882c0-2.22373 1.78113-2.576267 2.83773-2.576267h11.29058c1.0566 0 2.8377.325427 2.8377 2.549157v4.44745c-1.6302.1356-2.9283 1.35594-2.9283 2.87454v.678h-11.10941v-.678c0-1.5186-1.29811-2.73894-2.9283-2.87454zm14.00751 8.94918v3.3627h-11.07921v-3.3627zm-12.13582 10.5491h-1.11698v-1.844h1.11698zm14.33962 0h-1.117v-1.844h1.117zm3.2-4.9084h-.0302c0 .9491-.3924 2.1152-2.3547 2.1152h-.8151-1.117-13.22262-1.11698-.81509c-1.0566 0-2.35472-.3525-2.35472-2.1152v-7.295c0-1.05759.96604-1.92539 2.1434-1.92539s2.14339.8678 2.14339 1.92539v5.5051c0 .2712.24151.4882.5434.4882h12.16602c.3019 0 .5434-.217.5434-.4882v-5.5051c0-1.05759.9661-1.92539 2.1434-1.92539 1.1774 0 2.1434.8678 2.1434 1.92539z"
      fill={color}
    />
  </svg>
);

export const IconOffice: React.FC<IIcon> = ({ color = "#009f77" }) => (
  <svg fill="none" height="24" viewBox="0 0 24 24" width="24" xmlns="http://www.w3.org/2000/svg">
    <path
      clipRule="evenodd"
      d="m6.681 13h10.638c.442 0 .86-.193 1.145-.531.286-.337.408-.78.335-1.216l-1.667-10c-.12-.726-.742-1.253-1.479-1.253h-7.306c-.737 0-1.359.527-1.479 1.254l-1.667 10c-.073.435.049.879.335 1.216s.703.53 1.145.53zm1.173-11.583c.04-.241.248-.417.493-.417h7.306c.245 0 .453.176.494.417l1.667 10c.024.148-.015.292-.112.406s-.233.177-.382.177h-10.639c-.15 0-.285-.063-.382-.177s-.136-.259-.112-.406zm-.354 22.583h4.4963.0037.0037 4.4963c.276 0 .5-.224.5-.5s-.224-.5-.5-.5h-4v-4h7c.276 0 .5-.224.5-.5v-1-1-2c0-.827.673-1.5 1.5-1.5.276 0 .5-.224.5-.5s-.224-.5-.5-.5c-1.378 0-2.5 1.122-2.5 2.5v.0015c-.4182-.3147-.9378-.5015-1.5-.5015h-11c-.56222 0-1.08182.1868-1.5.5015v-.0015c0-1.378-1.122-2.5-2.5-2.5-.276 0-.5.224-.5.5s.224.5.5.5c.827 0 1.5.673 1.5 1.5v3 .0015.9985c0 .276.224.5.5.5h7v4h-4c-.276 0-.5.224-.5.5s.224.5.5.5zm11.5-6v-.4986-.0014-1-.0024c-.0013-.8259-.6738-1.4976-1.5-1.4976h-11c-.827 0-1.5.673-1.5 1.5v1.5z"
      fill={color}
      fillRule="evenodd"
    />
  </svg>
);

export const IconLeave: React.FC<IIcon> = ({ color = "#51d0d3" }) => (
  <svg fill="none" height="24" viewBox="0 0 24 24" width="24" xmlns="http://www.w3.org/2000/svg">
    <path
      clipRule="evenodd"
      d="m18.75 1.875h2.1562c1.706 0 3.0938 1.38783 3.0938 3.09375v15.93745c0 1.706-1.3878 3.0938-3.0938 3.0938h-17.81245c-1.70592 0-3.09375-1.3878-3.09375-3.0938v-15.93745c0-1.70592 1.38783-3.09375 3.09375-3.09375h2.15625v-1.125c0-.414188.33577-.75.75-.75s.75.335812.75.75v1.125h10.5v-1.125c0-.414188.3358-.75.75-.75s.75.335812.75.75zm-13.5 1.22177h-2.15625c-.87881 0-2.02923.99317-2.02923 1.87198v1.78125h21.77418v-1.78125c0-.87881-1.0536-1.87198-1.9325-1.87198h-2.1562v1.02823c0 .41419-.3358.75-.75.75s-.75-.33581-.75-.75v-1.02823h-10.5v1.02823c0 .41419-.33577.75-.75.75s-.75-.33581-.75-.75zm-2.15625 19.74193h17.81245c.8789 0 2.0293-1.0536 2.0293-1.9325v-12.97072h-21.87098v12.97072c0 .8789 1.15042 1.9325 2.02923 1.9325zm3.87402-3.0967 4.25123-4.2512-4.16132-4.1613.6843-.6843 4.16132 4.1613 4.2341-4.2341.6842.6843-4.234 4.2341 4.324 4.324-.6843.6843-4.324-4.324-4.25123 4.2512z"
      fill={color}
      fillRule="evenodd"
    />
  </svg>
);

export const IconClient: React.FC<IIcon> = ({ color = "#ffcd73" }) => (
  <svg fill="none" height="24" viewBox="0 0 24 24" width="24" xmlns="http://www.w3.org/2000/svg">
    <path
      d="m23.5998 12.8889h-1.2268l-.7731-12.474672c-.0145-.233562-.1893-.41455306-.4001-.41422756h-2.4c-.2108-.0003255-.3855.18066556-.4.41422756l-.7732 12.474672h-.8266v-2.6667c0-.24524-.1792-.44446-.3999-.44446-.0911-.00017-.1793.03434-.2502.09749l-3.3499 2.97777v-2.6308c0-.24524-.1791-.44446-.4-.44446-.0909-.00017-.1791.03434-.25.09749l-3.34995 2.97777v-2.6308c0-.24524-.179-.44446-.3999-.44446-.09097-.00017-.1793.03434-.25005.09749l-3.34995 2.97777v-2.6308c-.00015-.24524-.1793-.4443-.40005-.4443-.07281 0-.14385.02197-.20611.06348l-4.000045 2.66672c-.1202633.0804-.193945.2247-.193945.3808v10.6668c0 .2454.17915.4443.400049.4443h23.199751c.2209 0 .4001-.1989.4001-.4443v-10.2223c0-.2456-.1792-.4445-.4001-.4445zm-4.4265-12.000059h1.653l.745 12.000059h-3.143zm-5.5735 22.222359v-5.3334h2.4v5.3334zm9.6001 0h-6.3999v-5.7779c0-.2453-.1793-.4443-.4002-.4443h-3.1999c-.2209 0-.4001.199-.4001.4443v5.7779h-11.999849v-9.9706l3.199949-2.1334v2.7706c0 .2456.17901.4445.3999.4448.09082 0 .17916-.0345.2502-.0977l3.34995-2.9777v2.6306c0 .2456.17886.4445.3999.4448.09082 0 .17901-.0345.25005-.0975l3.35-2.9777v2.6305c0 .2455.1788.4447.3999.4447.0908 0 .179-.0345.25-.0975l3.35-2.9777v2.186c0 .2456.1791.4445.4.4445h6.7998v9.3333z"
      fill={color}
    />
  </svg>
);

export const IconInfoBalloon: React.FC<IIcon> = ({
  color = "#fff",
  backgroundColor = "#200a74",
}) => (
  <svg fill="none" height="16" viewBox="0 0 16 16" width="16" xmlns="http://www.w3.org/2000/svg">
    <g clipRule="evenodd" fillRule="evenodd">
      <path
        d="m8 16c4.4183 0 8-3.5817 8-8 0-4.41828-3.5817-8.00000016-8-8.00000035-4.41828-.00000019-8.00000016 3.58172035-8.00000035 8.00000035l-.00000035 8z"
        fill={color}
      />
      <path
        d="m8 3c-.55228 0-1 .44772-1 1s.44772 1 1 1 1-.44772 1-1-.44772-1-1-1zm0 4c-.55228 0-1 .44771-1 1v4.6c0 .5523.44772 1 1 1s1-.4477 1-1v-4.6c0-.55228-.44772-1-1-1z"
        fill={backgroundColor}
      />
    </g>
  </svg>
);

export const IconProfile: React.FC<IIcon> = ({ color = "#fff" }) => (
  <svg fill="none" height="16" viewBox="0 0 15 16" width="15" xmlns="http://www.w3.org/2000/svg">
    <path
      clipRule="evenodd"
      d="m3.58158 3.95134c0 2.3163 1.73236 4.20437 3.87348 4.20437s3.87344-1.88807 3.87344-4.20437c0-2.25791-1.67393-3.95134-3.87344-3.95134s-3.87348 1.69343-3.87348 3.95134zm-3.58158 11.46476c0 .3309.253041.5839.583941.5839h13.742059c.3309 0 .584-.253.584-.5839 0-4.1266-2.6472-6.40393-7.45502-6.40393-4.80778 0-7.45498 2.27733-7.45498 6.40393z"
      fill={color}
      fillRule="evenodd"
    />
  </svg>
);

export const IconInfo: React.FC<IIcon> = ({ color = "#fff" }) => (
  <svg fill="none" height="16" viewBox="0 0 16 16" width="16" xmlns="http://www.w3.org/2000/svg">
    <path
      d="m8 16c-4.41828 0-8.00000054-3.5817-8.00000035-8 .00000019-4.41828 3.58172035-8.00000054 8.00000035-8.00000035 4.4183.00000019 8 3.58172035 8 8.00000035 0 4.4183-3.5817 8-8 8z"
      fill="#200a74"
    />
    <path
      clipRule="evenodd"
      d="m8 3c-.55228 0-1 .44772-1 1s.44772 1 1 1 1-.44772 1-1-.44772-1-1-1zm0 4c-.55228 0-1 .44771-1 1v4.6c0 .5523.44772 1 1 1s1-.4477 1-1v-4.6c0-.55228-.44772-1-1-1z"
      fill={color}
      fillRule="evenodd"
    />
  </svg>
);

export const IconLogoDark: React.FC<IIcon> = ({ color = "#200a74" }) => (
  <svg
    fill="none"
    height="409"
    viewBox="0 0 360 409"
    width="360"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      clipRule="evenodd"
      d="m46.249 0c-25.5416 0-46.24716203 20.7055-46.24716203 46.2471v242.7969h-.00183797v74.554c0 3.588.834028 16.744 13.7615 29.502 12.9274 12.758 25.4378 15.15 29.608 15.15 1.0251 0 15.6834-.024 37.7863-.061h.3011.2972c68.0199-.112 205.1799-.338 234.3429-.338 38.782 0 43.369-47.443 43.369-47.443s-27.106 16.346-45.454 16.346c-11.173 0-104.759-.148-177.944-.263l-.356-.001c-46.8424-.074-85.2442-.135-88.1724-.135-7.5063 0-14.1785-10.365-14.1785-15.947 0-5.581 2.9191-10.764 7.9233-13.555 3.3259-1.855 5.7308-2.301 6.9698-2.391h264.9668c25.541 0 46.247-20.706 46.247-46.248v-251.9668c0-25.5416-20.706-46.2472-46.247-46.2472zm4.6554 31.8941c-9.6882 0-17.542 7.8538-17.542 17.542v246.7849 16.345c0 1.094-.0427 1.902-.0732 2.48-.0415.784-.0606 1.146.0794 1.225.1199.067.3563-.073.795-.332 1.5643-.926 5.7006-3.373 16.2964-3.373h258.149c9.688 0 17.497-7.853 17.497-17.541v-245.5889c0-9.6882-7.854-17.542-17.542-17.542zm175.9526 93.6899c0 19.119-12.682 35.406-30.442 41.593v63.659c20.299-9.49 28.785-18.093 32.902-37.336h-8.155c-1.749 0-2.655-2.088-1.461-3.366l24.838-26.571c.79-.846 2.131-.846 2.922 0l24.838 26.571c1.194 1.278.288 3.366-1.461 3.366h-8.213c-3.441 23.637-10.016 35.506-26.594 50.891-18.005 13.139-34.195 20.333-54.211 20.333-20.017 0-37.994-5.607-56.714-20.333-17.145-15.971-23.971-27.863-27.4128-50.891h-8.531c-1.7495 0-2.6558-2.088-1.4611-3.366l24.8379-26.571c.79-.846 2.131-.846 2.922 0l24.838 26.571c1.194 1.278.288 3.366-1.462 3.366h-7.762c4.402 19.305 13.235 27.472 32.396 37.336v-64.115c-17.102-6.502-29.191-22.47-29.191-41.137 0-24.441 20.724-44.2538 46.289-44.2538 25.564 0 46.288 19.8128 46.288 44.2538zm-46.289 13.156c7.6 0 13.761-5.89 13.761-13.157 0-7.266-6.161-13.156-13.761-13.156-7.601 0-13.762 5.89-13.762 13.156 0 7.267 6.161 13.157 13.762 13.157z"
      fill={color}
      fillRule="evenodd"
    />
  </svg>
);

export const IconLogoLight: React.FC<IIcon> = ({ color = "#fff" }) => (
  <svg
    fill="none"
    height="409"
    viewBox="0 0 360 409"
    width="360"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      clipRule="evenodd"
      d="m46.249 0c-25.5416 0-46.24716203 20.7055-46.24716203 46.2471v242.7969h-.00183797v74.554c0 3.588.834028 16.744 13.7615 29.502 12.9274 12.758 25.4378 15.15 29.608 15.15 1.0251 0 15.6834-.024 37.7863-.061h.3011.2972c68.0199-.112 205.1799-.338 234.3429-.338 38.782 0 43.369-47.443 43.369-47.443s-27.106 16.346-45.454 16.346c-11.173 0-104.759-.148-177.944-.263l-.356-.001c-46.8424-.074-85.2442-.135-88.1724-.135-7.5063 0-14.1785-10.365-14.1785-15.947 0-5.581 2.9191-10.764 7.9233-13.555 3.3259-1.855 5.7308-2.301 6.9698-2.391h264.9668c25.541 0 46.247-20.706 46.247-46.248v-251.9668c0-25.5416-20.706-46.2472-46.247-46.2472zm4.6554 31.8941c-9.6882 0-17.542 7.8538-17.542 17.542v246.7849 16.345c0 1.094-.0427 1.902-.0732 2.48-.0415.784-.0606 1.146.0794 1.225.1199.067.3563-.073.795-.332 1.5643-.926 5.7006-3.373 16.2964-3.373h258.149c9.688 0 17.497-7.853 17.497-17.541v-245.5889c0-9.6882-7.854-17.542-17.542-17.542zm175.9526 93.6899c0 19.119-12.682 35.406-30.442 41.593v63.659c20.299-9.49 28.785-18.093 32.902-37.336h-8.155c-1.749 0-2.655-2.088-1.461-3.366l24.838-26.571c.79-.846 2.131-.846 2.922 0l24.838 26.571c1.194 1.278.288 3.366-1.461 3.366h-8.213c-3.441 23.637-10.016 35.506-26.594 50.891-18.005 13.139-34.195 20.333-54.211 20.333-20.017 0-37.994-5.607-56.714-20.333-17.145-15.971-23.971-27.863-27.4128-50.891h-8.531c-1.7495 0-2.6558-2.088-1.4611-3.366l24.8379-26.571c.79-.846 2.131-.846 2.922 0l24.838 26.571c1.194 1.278.288 3.366-1.462 3.366h-7.762c4.402 19.305 13.235 27.472 32.396 37.336v-64.115c-17.102-6.502-29.191-22.47-29.191-41.137 0-24.441 20.724-44.2538 46.289-44.2538 25.564 0 46.288 19.8128 46.288 44.2538zm-46.289 13.156c7.6 0 13.761-5.89 13.761-13.157 0-7.266-6.161-13.156-13.761-13.156-7.601 0-13.762 5.89-13.762 13.156 0 7.267 6.161 13.157 13.762 13.157z"
      fill={color}
      fillRule="evenodd"
    />
  </svg>
);

export const IconClose: React.FC<IIcon> = ({ color = "#200A74" }) => (
  <svg
    fill="none"
    height="18"
    viewBox="0 0 18 18"
    width="18"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      d="m1 1 16 16m-16 0 16-16"
      stroke={color}
      strokeWidth="1.5"
    />
  </svg>
);

export const IconArrowUp: React.FC<IIcon> = ({ color = "#200A74" }) => (
  <svg
    width="55"
    height="8"
    viewBox="0 0 10 8"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
     d="M1 7.00071L7 1.52344L13 7.00071"
     stroke={color}
     strokeWidth="1.5"
    />
  </svg>
);

export const IconArrowDown: React.FC<IIcon> = ({color = "#200A74"}) => (
  <svg
    width="55"
    height="9"
    viewBox="0 0 10 9"
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path
      d="M13 1.52273L7 7L1 1.52273"
      stroke={color}
      strokeWidth="1.5"
    />
  </svg>

)
