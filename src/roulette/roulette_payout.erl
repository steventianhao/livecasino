-module(roulette_payout).
-export([payout/1]).

-record(bet,{id,ratio,nums}).
-define(RED,#bet{id=4000,ratio=1,nums=[1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36]}).
-define(BLACK,#bet{id=4001,ratio=1,nums=[2, 4, 6, 8, 10, 11, 13, 15, 17, 20, 22, 24, 26, 28, 29, 31, 33, 35]}).

-define(ODD,#bet{id=4002,ratio=1,nums=[1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35]}).
-define(EVEN,#bet{id=4003,ratio=1,nums=[2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36]}).

-define(BIG,#bet{id=4004,ratio=1,nums=[19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36]}).
-define(SMALL,#bet{id=4005,ratio=1,nums=[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18]}).

-define(DOZEN1,#bet{id=4010,ratio=2,nums=[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]}).
-define(DOZEN2,#bet{id=4011,ratio=2,nums=[13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24]}).
-define(DOZEN3,#bet{id=4012,ratio=2,nums=[25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36]}).

-define(COLUMN1,#bet{id=4020,ratio=2,nums=[1, 4, 7, 10, 13, 16, 19, 22, 25, 28, 31, 34]}).
-define(COLUMN2,#bet{id=4021,ratio=2,nums=[2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35]}).
-define(COLUMN3,#bet{id=4022,ratio=2,nums=[3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36]}).

-define(STRAIGHT00,#bet{id=4030,ratio=35,nums=[0]}).
-define(STRAIGHT01,#bet{id=4031,ratio=35,nums=[1]}).
-define(STRAIGHT02,#bet{id=4032,ratio=35,nums=[2]}).
-define(STRAIGHT03,#bet{id=4033,ratio=35,nums=[3]}).
-define(STRAIGHT04,#bet{id=4034,ratio=35,nums=[4]}).
-define(STRAIGHT05,#bet{id=4035,ratio=35,nums=[5]}).
-define(STRAIGHT06,#bet{id=4036,ratio=35,nums=[6]}).
-define(STRAIGHT07,#bet{id=4037,ratio=35,nums=[7]}).
-define(STRAIGHT08,#bet{id=4038,ratio=35,nums=[8]}).
-define(STRAIGHT09,#bet{id=4039,ratio=35,nums=[9]}).
-define(STRAIGHT10,#bet{id=4040,ratio=35,nums=[10]}).
-define(STRAIGHT11,#bet{id=4041,ratio=35,nums=[11]}).
-define(STRAIGHT12,#bet{id=4042,ratio=35,nums=[12]}).
-define(STRAIGHT13,#bet{id=4043,ratio=35,nums=[13]}).
-define(STRAIGHT14,#bet{id=4044,ratio=35,nums=[14]}).
-define(STRAIGHT15,#bet{id=4045,ratio=35,nums=[15]}).
-define(STRAIGHT16,#bet{id=4046,ratio=35,nums=[16]}).
-define(STRAIGHT17,#bet{id=4047,ratio=35,nums=[17]}).
-define(STRAIGHT18,#bet{id=4048,ratio=35,nums=[18]}).
-define(STRAIGHT19,#bet{id=4049,ratio=35,nums=[19]}).
-define(STRAIGHT20,#bet{id=4050,ratio=35,nums=[20]}).
-define(STRAIGHT21,#bet{id=4051,ratio=35,nums=[21]}).
-define(STRAIGHT22,#bet{id=4052,ratio=35,nums=[22]}).
-define(STRAIGHT23,#bet{id=4053,ratio=35,nums=[23]}).
-define(STRAIGHT24,#bet{id=4054,ratio=35,nums=[24]}).
-define(STRAIGHT25,#bet{id=4055,ratio=35,nums=[25]}).
-define(STRAIGHT26,#bet{id=4056,ratio=35,nums=[26]}).
-define(STRAIGHT27,#bet{id=4057,ratio=35,nums=[27]}).
-define(STRAIGHT28,#bet{id=4058,ratio=35,nums=[28]}).
-define(STRAIGHT29,#bet{id=4059,ratio=35,nums=[29]}).
-define(STRAIGHT30,#bet{id=4060,ratio=35,nums=[30]}).
-define(STRAIGHT31,#bet{id=4061,ratio=35,nums=[31]}).
-define(STRAIGHT32,#bet{id=4062,ratio=35,nums=[32]}).
-define(STRAIGHT33,#bet{id=4063,ratio=35,nums=[33]}).
-define(STRAIGHT34,#bet{id=4064,ratio=35,nums=[34]}).
-define(STRAIGHT35,#bet{id=4065,ratio=35,nums=[35]}).
-define(STRAIGHT36,#bet{id=4066,ratio=35,nums=[36]}).

-define(STREET01,#bet{id=4071,ratio=11,nums=[0, 1, 2]}).
-define(STREET02,#bet{id=4072,ratio=11,nums=[0, 2, 3]}).
-define(STREET03,#bet{id=4073,ratio=11,nums=[1, 2, 3]}).
-define(STREET04,#bet{id=4074,ratio=11,nums=[4, 5, 6]}).
-define(STREET05,#bet{id=4075,ratio=11,nums=[7, 8, 9]}).
-define(STREET06,#bet{id=4076,ratio=11,nums=[10, 11, 12]}).
-define(STREET07,#bet{id=4077,ratio=11,nums=[13, 14, 15]}).
-define(STREET08,#bet{id=4078,ratio=11,nums=[16, 17, 18]}).
-define(STREET09,#bet{id=4079,ratio=11,nums=[19, 20, 21]}).
-define(STREET10,#bet{id=4080,ratio=11,nums=[22, 23, 24]}).
-define(STREET11,#bet{id=4081,ratio=11,nums=[25, 26, 27]}).
-define(STREET12,#bet{id=4082,ratio=11,nums=[28, 29, 30]}).
-define(STREET13,#bet{id=4083,ratio=11,nums=[31, 32, 33]}).
-define(STREET14,#bet{id=4084,ratio=11,nums=[34, 35, 36]}).

-define(SPLIT01,#bet{id=4101,ratio=17,nums=[0, 1]}).
-define(SPLIT02,#bet{id=4102,ratio=17,nums=[0, 2]}).
-define(SPLIT03,#bet{id=4103,ratio=17,nums=[0, 3]}).
-define(SPLIT04,#bet{id=4104,ratio=17,nums=[1, 4]}).
-define(SPLIT05,#bet{id=4105,ratio=17,nums=[2, 5]}).
-define(SPLIT06,#bet{id=4106,ratio=17,nums=[3, 6]}).
-define(SPLIT07,#bet{id=4107,ratio=17,nums=[4, 7]}).
-define(SPLIT08,#bet{id=4108,ratio=17,nums=[5, 8]}).
-define(SPLIT09,#bet{id=4109,ratio=17,nums=[6, 9]}).
-define(SPLIT10,#bet{id=4110,ratio=17,nums=[7, 10]}).
-define(SPLIT11,#bet{id=4111,ratio=17,nums=[8, 11]}).
-define(SPLIT12,#bet{id=4112,ratio=17,nums=[9, 12]}).
-define(SPLIT13,#bet{id=4113,ratio=17,nums=[10, 13]}).
-define(SPLIT14,#bet{id=4114,ratio=17,nums=[11, 14]}).
-define(SPLIT15,#bet{id=4115,ratio=17,nums=[12, 15]}).
-define(SPLIT16,#bet{id=4116,ratio=17,nums=[13, 16]}).
-define(SPLIT17,#bet{id=4117,ratio=17,nums=[14, 17]}).
-define(SPLIT18,#bet{id=4118,ratio=17,nums=[15, 18]}).
-define(SPLIT19,#bet{id=4119,ratio=17,nums=[16, 19]}).
-define(SPLIT20,#bet{id=4120,ratio=17,nums=[17, 20]}).
-define(SPLIT21,#bet{id=4121,ratio=17,nums=[18, 21]}).
-define(SPLIT22,#bet{id=4122,ratio=17,nums=[19, 22]}).
-define(SPLIT23,#bet{id=4123,ratio=17,nums=[20, 23]}).
-define(SPLIT24,#bet{id=4124,ratio=17,nums=[21, 24]}).
-define(SPLIT25,#bet{id=4125,ratio=17,nums=[22, 25]}).
-define(SPLIT26,#bet{id=4126,ratio=17,nums=[23, 26]}).
-define(SPLIT27,#bet{id=4127,ratio=17,nums=[24, 27]}).
-define(SPLIT28,#bet{id=4128,ratio=17,nums=[25, 28]}).
-define(SPLIT29,#bet{id=4129,ratio=17,nums=[26, 29]}).
-define(SPLIT30,#bet{id=4130,ratio=17,nums=[27, 30]}).
-define(SPLIT31,#bet{id=4131,ratio=17,nums=[28, 31]}).
-define(SPLIT32,#bet{id=4132,ratio=17,nums=[29, 32]}).
-define(SPLIT33,#bet{id=4133,ratio=17,nums=[30, 33]}).
-define(SPLIT34,#bet{id=4134,ratio=17,nums=[31, 34]}).
-define(SPLIT35,#bet{id=4135,ratio=17,nums=[32, 35]}).
-define(SPLIT36,#bet{id=4136,ratio=17,nums=[33, 36]}).
-define(SPLIT37,#bet{id=4137,ratio=17,nums=[1, 2]}).
-define(SPLIT38,#bet{id=4138,ratio=17,nums=[2, 3]}).
-define(SPLIT39,#bet{id=4139,ratio=17,nums=[4, 5]}).
-define(SPLIT40,#bet{id=4140,ratio=17,nums=[5, 6]}).
-define(SPLIT41,#bet{id=4141,ratio=17,nums=[7, 8]}).
-define(SPLIT42,#bet{id=4142,ratio=17,nums=[8, 9]}).
-define(SPLIT43,#bet{id=4143,ratio=17,nums=[10, 11]}).
-define(SPLIT44,#bet{id=4144,ratio=17,nums=[11, 12]}).
-define(SPLIT45,#bet{id=4145,ratio=17,nums=[13, 14]}).
-define(SPLIT46,#bet{id=4146,ratio=17,nums=[14, 15]}).
-define(SPLIT47,#bet{id=4147,ratio=17,nums=[16, 17]}).
-define(SPLIT48,#bet{id=4148,ratio=17,nums=[17, 18]}).
-define(SPLIT49,#bet{id=4149,ratio=17,nums=[19, 20]}).
-define(SPLIT50,#bet{id=4150,ratio=17,nums=[20, 21]}).
-define(SPLIT51,#bet{id=4151,ratio=17,nums=[22, 23]}).
-define(SPLIT52,#bet{id=4152,ratio=17,nums=[23, 24]}).
-define(SPLIT53,#bet{id=4153,ratio=17,nums=[25, 26]}).
-define(SPLIT54,#bet{id=4154,ratio=17,nums=[26, 27]}).
-define(SPLIT55,#bet{id=4155,ratio=17,nums=[28, 29]}).
-define(SPLIT56,#bet{id=4156,ratio=17,nums=[29, 30]}).
-define(SPLIT57,#bet{id=4157,ratio=17,nums=[31, 32]}).
-define(SPLIT58,#bet{id=4158,ratio=17,nums=[32, 33]}).
-define(SPLIT59,#bet{id=4159,ratio=17,nums=[34, 35]}).
-define(SPLIT60,#bet{id=4160,ratio=17,nums=[35, 36]}).

-define(CONNER01,#bet{id=4201,ratio=8,nums=[1, 2, 4, 5]}).
-define(CONNER02,#bet{id=4202,ratio=8,nums=[2, 3, 5, 6]}).
-define(CONNER03,#bet{id=4203,ratio=8,nums=[4, 5, 7, 8]}).
-define(CONNER04,#bet{id=4204,ratio=8,nums=[5, 6, 8, 9]}).
-define(CONNER05,#bet{id=4205,ratio=8,nums=[7, 8, 10, 11]}).
-define(CONNER06,#bet{id=4206,ratio=8,nums=[8, 9, 11, 12]}).
-define(CONNER07,#bet{id=4207,ratio=8,nums=[10, 11, 13, 14]}).
-define(CONNER08,#bet{id=4208,ratio=8,nums=[11, 12, 14, 15]}).
-define(CONNER09,#bet{id=4209,ratio=8,nums=[13, 14, 16, 17]}).
-define(CONNER10,#bet{id=4210,ratio=8,nums=[14, 15, 17, 18]}).
-define(CONNER11,#bet{id=4211,ratio=8,nums=[16, 17, 19, 20]}).
-define(CONNER12,#bet{id=4212,ratio=8,nums=[17, 18, 20, 21]}).
-define(CONNER13,#bet{id=4213,ratio=8,nums=[19, 20, 22, 23]}).
-define(CONNER14,#bet{id=4214,ratio=8,nums=[20, 21, 23, 24]}).
-define(CONNER15,#bet{id=4215,ratio=8,nums=[22, 23, 25, 26]}).
-define(CONNER16,#bet{id=4216,ratio=8,nums=[23, 24, 26, 27]}).
-define(CONNER17,#bet{id=4217,ratio=8,nums=[25, 26, 28, 29]}).
-define(CONNER18,#bet{id=4218,ratio=8,nums=[26, 27, 29, 30]}).
-define(CONNER19,#bet{id=4219,ratio=8,nums=[28, 29, 31, 32]}).
-define(CONNER20,#bet{id=4220,ratio=8,nums=[29, 30, 32, 33]}).
-define(CONNER21,#bet{id=4221,ratio=8,nums=[31, 32, 34, 35]}).
-define(CONNER22,#bet{id=4222,ratio=8,nums=[32, 33, 35, 36]}).

-define(SIXLINE01,#bet{id=4301,ratio=5,nums=[1, 2, 3, 4, 5, 6]}).
-define(SIXLINE02,#bet{id=4302,ratio=5,nums=[4, 5, 6, 7, 8, 9]}).
-define(SIXLINE03,#bet{id=4303,ratio=5,nums=[7, 8, 9, 10, 11, 12]}).
-define(SIXLINE04,#bet{id=4304,ratio=5,nums=[10, 11, 12, 13, 14, 15]}).
-define(SIXLINE05,#bet{id=4305,ratio=5,nums=[13, 14, 15, 16, 17, 18]}).
-define(SIXLINE06,#bet{id=4306,ratio=5,nums=[16, 17, 18, 19, 20, 21]}).
-define(SIXLINE07,#bet{id=4307,ratio=5,nums=[19, 20, 21, 22, 23, 24]}).
-define(SIXLINE08,#bet{id=4308,ratio=5,nums=[22, 23, 24, 25, 26, 27]}).
-define(SIXLINE09,#bet{id=4309,ratio=5,nums=[25, 26, 27, 28, 29, 30]}).
-define(SIXLINE10,#bet{id=4310,ratio=5,nums=[28, 29, 30, 31, 32, 33]}).
-define(SIXLINE11,#bet{id=4311,ratio=5,nums=[31, 32, 33, 34, 35, 36]}).

-define(ALL_BETS,[
?RED,?BLACK,?ODD,?EVEN,?BIG,?SMALL,?DOZEN1,?DOZEN2,?DOZEN3,?COLUMN1,?COLUMN2,?COLUMN3,
?STRAIGHT00,?STRAIGHT01,?STRAIGHT02,?STRAIGHT03,?STRAIGHT04,?STRAIGHT05,?STRAIGHT06,?STRAIGHT07,
?STRAIGHT08,?STRAIGHT09,?STRAIGHT10,?STRAIGHT11,?STRAIGHT12,?STRAIGHT13,?STRAIGHT14,?STRAIGHT15,
?STRAIGHT16,?STRAIGHT17,?STRAIGHT18,?STRAIGHT19,?STRAIGHT20,?STRAIGHT21,?STRAIGHT22,?STRAIGHT23,
?STRAIGHT24,?STRAIGHT25,?STRAIGHT26,?STRAIGHT27,?STRAIGHT28,?STRAIGHT29,?STRAIGHT30,?STRAIGHT31,
?STRAIGHT32,?STRAIGHT33,?STRAIGHT34,?STRAIGHT35,?STRAIGHT36,
?STREET01,?STREET02,?STREET03,?STREET04,?STREET05,?STREET06,?STREET07,?STREET08,?STREET09,?STREET10,
?STREET11,?STREET12,?STREET13,?STREET14,
?SPLIT01,?SPLIT02,?SPLIT03,?SPLIT04,?SPLIT05,?SPLIT06,?SPLIT07,?SPLIT08,?SPLIT09,?SPLIT10,
?SPLIT11,?SPLIT12,?SPLIT13,?SPLIT14,?SPLIT15,?SPLIT16,?SPLIT17,?SPLIT18,?SPLIT19,?SPLIT20,
?SPLIT21,?SPLIT22,?SPLIT23,?SPLIT24,?SPLIT25,?SPLIT26,?SPLIT27,?SPLIT28,?SPLIT29,?SPLIT30,
?SPLIT31,?SPLIT32,?SPLIT33,?SPLIT34,?SPLIT35,?SPLIT36,?SPLIT37,?SPLIT38,?SPLIT39,?SPLIT40,
?SPLIT41,?SPLIT42,?SPLIT43,?SPLIT44,?SPLIT45,?SPLIT46,?SPLIT47,?SPLIT48,?SPLIT49,?SPLIT50,
?SPLIT51,?SPLIT52,?SPLIT53,?SPLIT54,?SPLIT55,?SPLIT56,?SPLIT57,?SPLIT58,?SPLIT59,?SPLIT60,
?CONNER01,?CONNER02,?CONNER03,?CONNER04,?CONNER05,?CONNER06,?CONNER07,?CONNER08,?CONNER09,?CONNER10,
?CONNER11,?CONNER12,?CONNER13,?CONNER14,?CONNER15,?CONNER16,?CONNER17,?CONNER18,?CONNER19,?CONNER20,?CONNER21,?CONNER22,
?SIXLINE01,?SIXLINE02,?SIXLINE03,?SIXLINE04,?SIXLINE05,?SIXLINE06,?SIXLINE07,?SIXLINE08,?SIXLINE09,?SIXLINE10,?SIXLINE11
]).

-define(ALL_BET_CATS,[B#bet.id ||B<-?ALL_BETS]).

payout(Ball)->
	maps:from_list([{B#bet.id,B#bet.ratio+1}|| B<-?ALL_BETS,lists:member(Ball,B#bet.nums)]).