#How to launch ROB-MEN docker image

first clone ```rob-men`` current version

```git clone https://github.com/esm-ispm-unibe-ch/reporting-bias/```

```docker run -v [ABSOLUTPATH TO rob-men]:/srv/shiny-server -p 2828:3838 tosku/rob-men:1.3```
