--
-- 游戏数据库 --
-- 格式如下 --
--
--
-- 添加表，注释/添加xx字段
-- author --
-- YYYYMMDDHH（如：2017080901，格式和长度都不能更改）
--
--
-- 重要说明：
-- 1. 修改表字段时，内容必须在原表及所有对该表的修改语句之后，最好是加到文件尾
-- 2. 时间要使用修改当日的时间，不能随意写（升级脚本自动生成过程和版本管理均依赖于时间，写错将导致数据库无法正常维护！！！）
-- 3. 时间不可重复，如果当前小时已被占用，使用下一小时
-- 4. 较新时间的语句可以放在较旧时间的语句之前，但不推荐这么做
--
--


SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
START TRANSACTION;
SET time_zone = "+00:00";



--
-- 数据库： `mingri_local_150244`
--

-- --------------------------------------------------------

-- 添加表，账号信息表
-- daer
-- 2017080201

DROP TABLE IF EXISTS `db_tab_account`;
CREATE TABLE IF NOT EXISTS `db_tab_account`
(
    `acc_name`     varchar(64) NOT NULL COMMENT '账号名',
    `pass_word`    varchar(100) DEFAULT NULL COMMENT '密码',
    `role_id_list` mediumblob COMMENT '角色id列表',
    PRIMARY KEY (`acc_name`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='账号';

-- --------------------------------------------------------

-- 添加表，角色黑名单表
-- daer
-- 2017080202

DROP TABLE IF EXISTS `db_tab_acc_blacklist`;
CREATE TABLE IF NOT EXISTS `db_tab_acc_blacklist`
(
    `role_id`  bigint(20)  NOT NULL COMMENT '角色id',
    `acc_name` varchar(64) NOT NULL COMMENT '账号名',
    `timeout`  int(11) DEFAULT NULL COMMENT '过期时间',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='角色黑名单表';

-- --------------------------------------------------------

--
-- 新增表，活动怪物
-- xiewendong
-- 2020122701
-- 

DROP TABLE IF EXISTS `db_tab_activity_monster`;
CREATE TABLE IF NOT EXISTS `db_tab_activity_monster`
(
    `mon_key`           int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '怪物key',
    `mon_class`         tinyint(3) UNSIGNED NOT NULL DEFAULT '0' COMMENT '分类 ACTIVITY_MONSTER_TYPE',
    `activity_id`       int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '归属的活动配置表id',
    `zone_id`           int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '归属的地图模板区域id',
    `template_id`       int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '该区域的模板id(data_map_template)',
    `area_id`           int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '业务概念上的区域（例如1赛季的国家，三赛季的省或者市）',
    `ma_cfg_id`         int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT 'MonsterActivity表的配置id',
    `is_alive`          tinyint(3) UNSIGNED NOT NULL DEFAULT '0' COMMENT '是否存活',
    `relive_time`       int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '死亡后，下一次复活时间',
    `born_x`            int(11) UNSIGNED    NOT NULL DEFAULT '0',
    `born_y`            int(11) UNSIGNED    NOT NULL DEFAULT '0',
    `show_army_id_list` blob COMMENT '哪些军队是展示到地图上的',
    `army_id_list`      mediumblob COMMENT '所有军队id',
    `mon_param`         blob COMMENT '额外参数',
    PRIMARY KEY (`mon_key`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='活动怪物表';

-- --------------------------------------------------------

--
-- 添加表，竞技场活动商店信息
-- daer
-- 2021070701
--

DROP TABLE IF EXISTS `db_tab_activity_refresh_shop`;
CREATE TABLE IF NOT EXISTS `db_tab_activity_refresh_shop`
(
    `activity_id`       int(11) NOT NULL COMMENT '活动ID',
    `shop_id`           int(11) NOT NULL COMMENT '商店ID（对应ActivityRefreshShop表ID）',
    `good_list`         blob COMMENT '物品列表',
    `last_refresh_time` int(11) NOT NULL COMMENT '商店上次自动刷新时间',
    `random_data`       mediumblob COMMENT '本轮已随机过的商品ID以及槽位',
    PRIMARY KEY (`activity_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='竞技场活动商店信息';

-- --------------------------------------------------------

--
-- 添加表，禁言表
-- zsk
-- 2018111301
--

DROP TABLE IF EXISTS `db_tab_ban_talk`;
CREATE TABLE IF NOT EXISTS `db_tab_ban_talk`
(
    `role_id` bigint(20) NOT NULL COMMENT '角色id',
    `timeout` int(11)    NOT NULL COMMENT '过期时间',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='禁言表';

-- --------------------------------------------------------

--
-- 添加表，战报
-- lei
-- 2019111122
--

DROP TABLE IF EXISTS `db_tab_battle_report`;
CREATE TABLE IF NOT EXISTS `db_tab_battle_report`
(
    `report_id`          bigint(20) NOT NULL COMMENT '战报id',
    `role_id`            bigint(20) NOT NULL COMMENT '所属id',
    `battle_start_time`  int(11)    NOT NULL COMMENT '战斗开始时间',
    `battle_end_time`    int(11)    NOT NULL COMMENT '战斗结束时间',
    `report_time`        int(11)    NOT NULL COMMENT '战报时间',
    `big_type`           int(11)    NOT NULL COMMENT '战报大类',
    `report_type`        int(11)    NOT NULL COMMENT '战报类型',
    `pos_args`           mediumblob NOT NULL COMMENT '战斗位置参数',
    `title_args`         mediumblob NOT NULL COMMENT '标题参数',
    `self_list`          mediumblob NOT NULL COMMENT '我军参战单位列表',
    `friend_list`        mediumblob NOT NULL COMMENT '友军参战单位列表',
    `enemy_list`         mediumblob NOT NULL COMMENT '敌军参战单位列表',
    `result`             int(11)    NOT NULL COMMENT '结果',
    `is_collect`         int(11)    NOT NULL COMMENT '是否收藏',
    `is_union_city`      int(11)    NOT NULL COMMENT '是否攻打联盟城',
    `report_detail_list` longblob   NOT NULL COMMENT '战斗详情列表',
    `static_list`        longblob   NOT NULL COMMENT '数据统计',
    `city_info`          mediumblob NOT NULL COMMENT '城池信息',
    `rank_list`          mediumblob NOT NULL COMMENT '攻城排行',
    PRIMARY KEY (`report_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='战报表';

-- --------------------------------------------------------

-- 修改表，角色表加字段
-- djc
-- 2020080616
--

DROP TABLE IF EXISTS `db_tab_chat_report`;
CREATE TABLE IF NOT EXISTS `db_tab_chat_report`
(
    `role_id`        bigint(20)  NOT NULL COMMENT '被举报者角色id',
    `time`           int(11)     NOT NULL COMMENT '举报时间',
    `acc_name`       varchar(64) NOT NULL COMMENT '被举报者账号名',
    `server_num`     int(11)     NOT NULL COMMENT '被举报者服务器id',
    `content`        int(11)     NOT NULL COMMENT '被举报者内容',
    `type_list`      mediumblob COMMENT '举报类型',
    `report_content` mediumblob COMMENT '举报原因',
    `report_id`      bigint(20)  NOT NULL COMMENT '举报者id',
    PRIMARY KEY (`role_id`, `time`, `report_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='聊天举报';

-- --------------------------------------------------------

--
-- 添加表，城战排行榜历史
-- Karl
-- 2020061915
--

DROP TABLE IF EXISTS `db_tab_city_battle_rank`;
CREATE TABLE IF NOT EXISTS `db_tab_city_battle_rank`
(
    `city_id`          bigint(20)       NOT NULL DEFAULT '0' COMMENT '城池id',
    `last_battle_rank` mediumblob COMMENT '最近一次攻城战的排行榜',
    `time`             int(11) UNSIGNED NOT NULL DEFAULT '0' COMMENT '时间',
    PRIMARY KEY (`city_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='城战排行榜历史';

-- --------------------------------------------------------

--
-- 添加表，君主任务表
-- hyq
-- 2019112701
--

DROP TABLE IF EXISTS `db_tab_city_monarch_task`;
CREATE TABLE IF NOT EXISTS `db_tab_city_monarch_task`
(
    `city_id`           bigint(20) NOT NULL COMMENT '城池id',
    `monarch_task_list` mediumblob COMMENT '君主任务列表',
    PRIMARY KEY (`city_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='城池君主任务列表';

-- --------------------------------------------------------

--
-- 添加表，零散的公共数据
-- daer
-- 2017090301
--

DROP TABLE IF EXISTS `db_tab_common_data`;
CREATE TABLE IF NOT EXISTS `db_tab_common_data`
(
    `key` int(11)    NOT NULL COMMENT 'key',
    `val` mediumblob NOT NULL COMMENT 'val',
    PRIMARY KEY (`key`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='零散的公共数据';

-- --------------------------------------------------------

--
-- 添加表，国家定时科技
-- lei
-- 2019121016
--

DROP TABLE IF EXISTS `db_tab_country_time_effect`;
CREATE TABLE IF NOT EXISTS `db_tab_country_time_effect`
(
    `country_id`  int(11) NOT NULL COMMENT '国家id',
    `effect_list` mediumblob COMMENT '列表',
    PRIMARY KEY (`country_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='国家定时科技';

-- --------------------------------------------------------

--
-- 新增表，武将已拥有的图鉴
-- djc
-- 2020092911
--

DROP TABLE IF EXISTS `db_tab_general_book`;
CREATE TABLE IF NOT EXISTS `db_tab_general_book`
(
    `role_id`       bigint(20) NOT NULL COMMENT '角色id',
    `general_types` mediumblob COMMENT '武将type列表',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='武将已拥有的图鉴';

-- --------------------------------------------------------

--
-- 添加表，地块进化物
-- daer
-- 2019032601
--

DROP TABLE IF EXISTS `db_tab_grid_evolution`;
CREATE TABLE IF NOT EXISTS `db_tab_grid_evolution`
(
    `index`           int(11)    NOT NULL COMMENT '坐标索引',
    `type`            int(11)    NOT NULL COMMENT '类型',
    `shape_index`     int(11)    NOT NULL COMMENT '坐标索引',
    `is_occupy`       tinyint(4) NOT NULL COMMENT '是否被占领',
    `army_id`         bigint(20) NOT NULL COMMENT '军队id',
    `general_id_list` mediumblob COMMENT 'monteam随机出来的武将id列表',
    `progress`        int(11)    NOT NULL COMMENT '进程',
    `start_time`      int(11)    NOT NULL COMMENT '当前进度的开始时间',
    `city_id`         bigint(20) NOT NULL COMMENT '是否被占领',
    `config_id`       int(11)    NOT NULL COMMENT '是否被占领',
    `expire_time`     int(11)    NOT NULL COMMENT '过期时间戳',
    `event_id`        bigint(20) NOT NULL COMMENT '过期时间戳',
    `args`            mediumblob COMMENT '额外的数据',
    `city_lv`         int(11) DEFAULT NULL COMMENT '都府等级',
    PRIMARY KEY (`index`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='地块进化物';

-- --------------------------------------------------------

--
-- 添加表，土地掠夺
-- daer
-- 2021070702
--

DROP TABLE IF EXISTS `db_tab_grid_rob`;
CREATE TABLE IF NOT EXISTS `db_tab_grid_rob`
(
    `shape_id`       int(11) UNSIGNED    NOT NULL COMMENT '资源地id',
    `shape_index`    int(11) UNSIGNED    NOT NULL COMMENT '资源地index',
    `owner_id`       bigint(20) UNSIGNED NOT NULL COMMENT '归属者id',
    `robber_id`      bigint(20) UNSIGNED NOT NULL COMMENT '掠夺者id',
    `recover_list`   mediumblob          NOT NULL COMMENT '正在收复掠夺的军队列表，[{army_id,start_time}]',
    `process`        int(11) UNSIGNED    NOT NULL COMMENT '收复进度',
    `battle_outcome` mediumblob          NOT NULL COMMENT '占领土地战斗相关数据，#{}',
    `start_time`     int(11) UNSIGNED    NOT NULL COMMENT '掠夺起始时间戳',
    `end_time`       int(11) UNSIGNED    NOT NULL COMMENT '掠夺结束时间戳',
    PRIMARY KEY (`shape_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='土地掠夺';

-- --------------------------------------------------------

--
-- 添加表，群聊表
-- djc
-- 2020052714
--

DROP TABLE IF EXISTS `db_tab_group_chat`;
CREATE TABLE IF NOT EXISTS `db_tab_group_chat`
(
    `id`         bigint(20)   NOT NULL COMMENT 'id',
    `leader_id`  bigint(20)   NOT NULL COMMENT '群主id',
    `name`       varchar(100) NOT NULL COMMENT '名称',
    `member_ids` mediumblob COMMENT '成员id列表',
    `chat_log`   mediumblob COMMENT '聊天记录',
    PRIMARY KEY (`id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='群聊表';

-- --------------------------------------------------------

--
-- 新增表，合纵投票
-- xiewendong
-- 2020092201
--

DROP TABLE IF EXISTS `db_tab_hezong_vote`;
CREATE TABLE IF NOT EXISTS `db_tab_hezong_vote`
(
    `role_id`    bigint(20)          NOT NULL COMMENT '角色id',
    `country`    smallint(6)         NOT NULL COMMENT '开始投票时的国家',
    `vote_time`  int(10) UNSIGNED    NOT NULL COMMENT '投票时间',
    `option`     tinyint(3) UNSIGNED NOT NULL DEFAULT '0' COMMENT '选项 0-未选择 1-同意 2-反对 3-弃权',
    `result_saw` tinyint(3) UNSIGNED NOT NULL DEFAULT '0' COMMENT '是否看过结果 0-未看过 1-已看过',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='合纵投票表';

-- --------------------------------------------------------

-- 添加表，ip黑名单表
-- daer
-- 2017080203

DROP TABLE IF EXISTS `db_tab_ip_blacklist`;
CREATE TABLE IF NOT EXISTS `db_tab_ip_blacklist`
(
    `ip`      varchar(30) NOT NULL COMMENT 'ip',
    `timeout` int(11) DEFAULT NULL COMMENT '过期时间',
    PRIMARY KEY (`ip`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='ip黑名单表';

-- --------------------------------------------------------

--
-- 添加表，场景军队表
-- lei
-- 2017091701
--

DROP TABLE IF EXISTS `db_tab_map_army`;
CREATE TABLE IF NOT EXISTS `db_tab_map_army`
(
    `army_id`                  bigint(20) NOT NULL COMMENT '军队id',
    `kind`                     int(11)             DEFAULT NULL COMMENT '军队类型，人的军队，怪物的军队，系统的军队',
    `status`                   int(11)             DEFAULT NULL COMMENT '状态',
    `pos_data`                 mediumblob COMMENT '位置信息',
    `move_data`                mediumblob COMMENT '移动信息',
    `next_oper`                mediumblob COMMENT '下一步动作',
    `array_id_list`            mediumblob COMMENT '方阵id列表',
    `leader_id`                bigint(20)          DEFAULT NULL COMMENT '军队的领队id，方阵死亡时不会变化',
    `dead_array_id_list`       mediumblob COMMENT '死亡的方阵列表',
    `move_speed`               int(11)             DEFAULT NULL COMMENT '移动速度',
    `building_id`              bigint(20)          DEFAULT NULL COMMENT '建筑id',
    `fortress_id`              bigint(20)          DEFAULT NULL COMMENT '要塞id',
    `camp_id`                  bigint(20)          DEFAULT NULL COMMENT '营寨id',
    `train_info`               mediumblob COMMENT '屯田练兵信息',
    `last_battle_end_time`     int(11)             DEFAULT NULL COMMENT '上次结束战斗的时间',
    `extend_args`              mediumblob COMMENT '扩展数据',
    `army_mon`                 mediumblob COMMENT '当军队是怪物类型时的怪物数据',
    `army_role`                mediumblob COMMENT '当军队属于玩家时的数据',
    `fighting`                 int(11)             DEFAULT NULL COMMENT '战力',
    `fatigue_goods`            bigint(20)          DEFAULT NULL COMMENT '疲劳值增益道具',
    `fatigue_goods_time`       int(11)             DEFAULT NULL COMMENT '疲劳值增益道具有效时间(时间戳)',
    `fatigue_goods_add_time`   int(11)             DEFAULT NULL COMMENT '疲劳值增益道具上次恢复时间(时间戳)',
    `vehicle_energy_last_time` int(11)             DEFAULT NULL COMMENT '载具部队上次扣除能量值时间(时间戳)',
    `hurt_time`                int(11)             DEFAULT NULL COMMENT '重伤持续时间（时间戳）',
    `union_args`               mediumblob COMMENT '军队联盟相关信息 #r_map_army_union{}',
    `show_name_args`           mediumblob COMMENT '守军显示名称相关参数 #r_army_show_name_args{}',
    `last_elude_time`          int(11)             DEFAULT '0' COMMENT '上次鸣金的时间',
    `coll_info`                mediumblob COMMENT '采集物相关信息 #r_map_coll_simple{}',
    `is_vehicle_army`          tinyint(4) NOT NULL DEFAULT '0' COMMENT '是否载具部队',
    `is_vehicle_deformation`   tinyint(4) NOT NULL DEFAULT '0' COMMENT '载具部队变形是否变形中',
    `vehicle_id`               int(11)    NOT NULL DEFAULT '0' COMMENT '载具配置表id(载具部队才有)',
    `vehicle_building_id`      bigint(20) NOT NULL DEFAULT '0' COMMENT '载具部队对应的建筑id',
    `deformation_building_id`  bigint(20) NOT NULL DEFAULT '0' COMMENT '载具部队变形后对应的建筑id',
    PRIMARY KEY (`army_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='场景军队表';

-- --------------------------------------------------------

--
-- 添加表，军队扩展信息
-- daer
-- 2021070703
--

DROP TABLE IF EXISTS `db_tab_map_army_ei`;
CREATE TABLE IF NOT EXISTS `db_tab_map_army_ei`
(
    `army_id`    bigint(20) NOT NULL COMMENT '军队id',
    `army_skill` mediumblob COMMENT '军队技能, [{SkillType, Level}]',
    PRIMARY KEY (`army_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='军队扩展信息，只用于角色军队';

-- --------------------------------------------------------

--
-- 添加表，方阵表
-- daer
-- 2017080701
--

DROP TABLE IF EXISTS `db_tab_map_array`;
CREATE TABLE IF NOT EXISTS `db_tab_map_array`
(
    `array_id`           bigint(20)   NOT NULL COMMENT '方阵id',
    `kind`               mediumint(9) NOT NULL COMMENT '类型',
    `show_type`          mediumint(9) NOT NULL COMMENT '怪物显示类型',
    `army_id`            bigint(20)   NOT NULL COMMENT '军队id',
    `general_type`       int(11)      NOT NULL,
    `general_profession` int(11)      NOT NULL,
    `general_star`       int(11)      NOT NULL COMMENT '武将星级',
    `level`              mediumint(9) NOT NULL COMMENT '等级',
    `status`             tinyint(4)   NOT NULL COMMENT '状态',
    `pos_data`           mediumblob COMMENT '位置信息',
    `move_data`          mediumblob COMMENT '行走信息',
    `extend_args`        mediumblob COMMENT '额外的信息',
    `base_attr`          mediumblob COMMENT 'base属性',
    `attr`               mediumblob COMMENT '属性',
    `skill_list`         mediumblob COMMENT '技能列表',
    `buff_list`          mediumblob COMMENT 'buff列表',
    `fighting`           int(11)    DEFAULT NULL COMMENT '战力',
    `fatigue`            int(11)    DEFAULT NULL COMMENT '疲劳值',
    `soldier_lvl`        int(11)    DEFAULT NULL COMMENT '士兵等级',
    `origin_attr`        mediumblob COMMENT '原始属性',
    `awaken_num`         int(11)    DEFAULT NULL COMMENT '觉醒次数',
    `camp_pos`           tinyint(4) DEFAULT NULL COMMENT '军营中位置(1,2,3)',
    `role_misc`          mediumblob COMMENT '玩家方阵相关数据',
    `leader_array_id`    bigint(20)   NOT NULL COMMENT '领头方阵id',
    PRIMARY KEY (`array_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='方阵';

-- --------------------------------------------------------

--
-- 添加表，世界boss
-- daer
-- 2021070704
--

DROP TABLE IF EXISTS `db_tab_map_boss`;
CREATE TABLE IF NOT EXISTS `db_tab_map_boss`
(
    `pos`          varchar(255) NOT NULL COMMENT 'boss怪物出生坐标',
    `boss_tid`     int(11)      NOT NULL COMMENT 'MonsterBoss的id',
    `army_id_list` blob COMMENT '该boss所包含的军队id列表',
    `province`     int(11)      NOT NULL COMMENT '省',
    `born_time`    int(11)      NOT NULL COMMENT '出生时间',
    `grad`         int(11)      NOT NULL COMMENT 'boss等级类型，见MonsterBoss的grad字段',
    PRIMARY KEY (`pos`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='世界boss';

-- --------------------------------------------------------

-- 添加表，建筑表
-- daer
-- 2017081201
--

DROP TABLE IF EXISTS `db_tab_map_building`;
CREATE TABLE IF NOT EXISTS `db_tab_map_building`
(
    `building_id`         bigint(20)          NOT NULL COMMENT '实例id',
    `role_id`             bigint(20)          NOT NULL COMMENT '角色id',
    `city_id`             bigint(20)          NOT NULL COMMENT '城池id',
    `type`                int(11)             NOT NULL COMMENT 'tid',
    `big_type`            int(11)             NOT NULL COMMENT '大类',
    `level`               int(11)             NOT NULL COMMENT '等级',
    `x`                   int(11)             NOT NULL COMMENT 'x',
    `y`                   int(11)             NOT NULL COMMENT 'y',
    `state`               int(11)             NOT NULL COMMENT '自身状态',
    `attr`                mediumblob          NOT NULL COMMENT '属性',
    `direction`           int(11)             NOT NULL COMMENT '方向',
    `repair_type`         int(11)             NOT NULL COMMENT '维修类型',
    `auto_repair_time`    int(11)             NOT NULL COMMENT '自动维修时间',
    `add_repair_speed`    int(11)             NOT NULL COMMENT '增加的维修速度',
    `build_start_time`    int(11)             NOT NULL COMMENT '开始建造时间',
    `up_level_time`       int(11)             NOT NULL COMMENT '升级时间',
    `auto_grow_time`      int(11)             NOT NULL COMMENT '自动成长的时间',
    `out_war_sec`         int(11)             NOT NULL COMMENT '退出战斗的时间',
    `enter_war_sec`       int(11)             NOT NULL COMMENT '进入战斗的时间',
    `remould_start_time`  int(11)             NOT NULL COMMENT '开始改造时间戳',
    `remould_type`        int(11)             NOT NULL COMMENT '改造成哪个小类型的建筑',
    `remould_level`       int(11)             NOT NULL COMMENT '改造成的建筑的等级',
    `open`                int(11)             NOT NULL COMMENT '是否开启',
    `name`                varchar(20)         NOT NULL COMMENT '名字',
    `defend_army_list`    mediumblob          NOT NULL COMMENT '防守部队列表',
    `assist_army_list`    mediumblob          NOT NULL COMMENT '支援部队列表',
    `defend_army_limit`   int(11)             NOT NULL COMMENT '防守部队数量限制',
    `battle_state`        int(11)             NOT NULL COMMENT '战斗状态',
    `is_can_attack`       tinyint(4)          NOT NULL COMMENT '能否攻击其他对象',
    `is_can_attacked`     tinyint(4)          NOT NULL COMMENT '能否被攻击',
    `skill_list`          mediumblob          NOT NULL COMMENT '技能列表',
    `cover_pos_list`      mediumblob          NOT NULL COMMENT '覆盖范围',
    `wall_type`           int(11)             NOT NULL COMMENT '城墙类型',
    `belong_id`           bigint(20)          NOT NULL COMMENT '所属id',
    `army_id`             bigint(20)          NOT NULL COMMENT '军队id',
    `extend_func`         mediumblob          NOT NULL COMMENT '扩展内容',
    `union_building_args` mediumblob          NOT NULL COMMENT '仅联盟npc城池建筑使用',
    `city_tid`            int(11)             NOT NULL COMMENT '建筑所属城池tid，city表id，野外建筑为0',
    `patrol_ing_army`     bigint(20)          NOT NULL COMMENT '巡逻兵营巡逻中的部队',
    `is_active`           tinyint(3) UNSIGNED NOT NULL DEFAULT '1' COMMENT '是否活跃数据',
    `visible_type`        int(11)             NOT NULL COMMENT '城外陷阱可见类型',
    `group_id`            int(11)             NOT NULL COMMENT '建筑所属的建筑群id',
    `auto_del_time`       int(11)             NOT NULL DEFAULT '0' COMMENT '建筑自动删除时间',
    `auto_useless_time`   int(11)             NOT NULL DEFAULT '0' COMMENT '建筑自动损坏时间',
    `enter_damage_time`   int(10) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '建筑进入损坏状态的时间',
    `city_supply_type`    int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '野外建筑临时的城市补给类型',
    `vehicle_id`          int(11)             NOT NULL DEFAULT '0' COMMENT '载具配置表id(临时要塞才有)',
    `vehicle_army_id`     bigint(20)          NOT NULL DEFAULT '0' COMMENT '载具部队id(临时要塞才有)',
    `save_values`         longblob            NOT NULL COMMENT '需要永久保存的杂项数据',
    PRIMARY KEY (`building_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='场景建筑';

-- --------------------------------------------------------

--
-- 添加表，城池
-- daer
-- 2017081101
--

DROP TABLE IF EXISTS `db_tab_map_city`;
CREATE TABLE IF NOT EXISTS `db_tab_map_city`
(
    `city_id`                  bigint(20)          NOT NULL COMMENT '城池id',
    `tid`                      int(11)             NOT NULL COMMENT '城池类型',
    `city_name_text_id`        int(11)             NOT NULL COMMENT 'npc城池名对应的text_id',
    `city_name`                varchar(20)         NOT NULL COMMENT '城池名字',
    `host_id`                  bigint(20)          NOT NULL COMMENT '属主id',
    `tmp_host_id`              bigint(20)          NOT NULL COMMENT '临时属主id',
    `x`                        int(11)             NOT NULL COMMENT 'x',
    `y`                        int(11)             NOT NULL COMMENT 'y',
    `group_id`                 int(11)             NOT NULL COMMENT '组id',
    `building_id_list`         mediumblob COMMENT '建筑列表',
    `king_building_id`         bigint(20)          NOT NULL COMMENT '大本营建筑id',
    `country_id`               int(11)             NOT NULL COMMENT '城池所属国家id',
    `center_grid_level`        int(11)             NOT NULL COMMENT '城池中心地块等级',
    `prosp`                    int(11)             NOT NULL COMMENT '城池繁荣度',
    `prosp_change_list`        mediumblob COMMENT '[{index, time}], 繁荣度变为index的时间，分段记录',
    `prosp_dec_time`           bigint(20)          NOT NULL COMMENT '大于0，上一次城池大本营被损坏，繁荣度减少时间戳。0不在CD时间',
    `peace`                    int(11)             NOT NULL COMMENT '城池治安值',
    `peace_change_list`        mediumblob COMMENT '[{value, time}], 治安值变为value的时间，分段记录',
    `event_effect_list`        mediumblob COMMENT '城市事件效果列表',
    `event_log_list`           mediumblob COMMENT '城市事件日志列表',
    `last_continue_dec_time`   bigint(20)          NOT NULL COMMENT '上一次持续扣除治安值时间戳。永远大于0',
    `last_battle_dec_time`     bigint(20)          NOT NULL COMMENT '大于0，上一次发生战斗时减少治安值时间戳。0不在CD时间',
    `trade`                    int(11)             NOT NULL COMMENT '商业值',
    `farm`                     int(11)             NOT NULL COMMENT '农业值',
    `office_list`              mediumblob COMMENT '城池已开放的官职列表，未开放的官职不会出现。[#city_office{}, ...]',
    `patrol`                   mediumblob COMMENT '城池巡查信息',
    `policy`                   mediumblob COMMENT '城池政策信息',
    `rebel_hold`               mediumblob COMMENT '城池叛军据点信息',
    `is_can_expand`            tinyint(4)          NOT NULL COMMENT '城池是否可扩建。0不可扩建；1可扩建',
    `business_num`             int(11)                      DEFAULT NULL COMMENT '经营次数',
    `business_time`            int(11)                      DEFAULT NULL COMMENT '发起经营时间',
    `be_att_time_list`         mediumblob COMMENT '城池被攻击时间列表',
    `protect_type`             tinyint(4)                   DEFAULT NULL COMMENT '城池免战',
    `protect_start_time`       int(11)                      DEFAULT NULL COMMENT '免战开始时间',
    `protect_end_time`         int(11)                      DEFAULT NULL COMMENT '免战结束时间',
    `extend`                   mediumblob COMMENT '扩展字段,以key-value的形式存储,使用前要先定义',
    `newplayer_mon_list`       mediumblob COMMENT '新手怪完成的列表',
    `battle_union_id_list`     mediumblob COMMENT '对该城池宣战的联盟id列表',
    `role_village_task_list`   mediumblob COMMENT '玩家的村庄任务列表',
    `public_village_task_list` mediumblob COMMENT '公共村庄任务列表',
    `village_mon_army_list`    mediumblob COMMENT '前置任务的怪物军队id列表',
    `cousin_city_list`         mediumblob COMMENT '同属于一个组的城池',
    `center_city_id`           bigint(20)          NOT NULL COMMENT '中心城id',
    `next_rogue_refresh_time`  int(11)             NOT NULL COMMENT '下次流寇营地刷新的时间',
    `secret`                   mediumblob COMMENT '城池密探信息',
    `npc_task`                 mediumblob COMMENT 'npc任务信息（目前只有玩家主城、营寨城池有）',
    `first_occupy`             bigint(20)                   DEFAULT NULL COMMENT '第一次给了谁',
    `is_active`                tinyint(3) UNSIGNED NOT NULL DEFAULT '1' COMMENT '是否活跃数据',
    `skin_id`                  int(11)                      DEFAULT NULL COMMENT '城池皮肤id',
    `declare`                  varchar(512)        NOT NULL COMMENT '自定义宣言',
    `fly_away`                 mediumblob COMMENT '战中迁移',
    `role_village_cache_task`  mediumblob COMMENT '玩家村庄任务缓存',
    `create_status`            smallint(6)                  DEFAULT NULL COMMENT '城池建造状态',
    `strategy_cd_end_time`     int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '城池紧急备战CD结束时间',
    PRIMARY KEY (`city_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='城池';

-- --------------------------------------------------------

--
-- 添加表，采集物表
-- Tom
-- 2021011302
--

DROP TABLE IF EXISTS `db_tab_map_coll`;
CREATE TABLE IF NOT EXISTS `db_tab_map_coll`
(
    `coll_id`      bigint(20) UNSIGNED NOT NULL COMMENT '采集物唯一ID',
    `tid`          int(11) UNSIGNED    NOT NULL COMMENT '采集物配置id',
    `x`            int(11) UNSIGNED    NOT NULL COMMENT 'x坐标',
    `y`            int(11) UNSIGNED    NOT NULL COMMENT 'y坐标',
    `role_id`      bigint(20) UNSIGNED NOT NULL COMMENT '归属者id',
    `drop_list`    mediumblob          NOT NULL COMMENT '掉落物列表，[{资源ID,数量} |_]',
    `protect_time` int(11) UNSIGNED    NOT NULL COMMENT '保护期截止时间戳，在此时间前只能归属者采集',
    `die_time`     int(11) UNSIGNED    NOT NULL COMMENT '消失时间戳',
    `army_list`    mediumblob          NOT NULL COMMENT '当前正在采集中的部队列表，[{army_id, start_time} | _]',
    PRIMARY KEY (`coll_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='采集物表';

-- --------------------------------------------------------

--
-- 添加表，地图格子数据
-- daer
-- 2017080801
--

DROP TABLE IF EXISTS `db_tab_map_grid`;
CREATE TABLE IF NOT EXISTS `db_tab_map_grid`
(
    `index`               int(11)     NOT NULL COMMENT '序号',
    `country`             smallint(6) NOT NULL COMMENT '所属国家',
    `province`            int(11)     NOT NULL COMMENT '省',
    `landform_floor_type` int(11)     NOT NULL COMMENT '地形和地表层类型',
    `res_type_level`      int(11)     NOT NULL COMMENT '资源层类型和等级',
    `res_shape_id`        int(11)     NOT NULL COMMENT '所属资源团的编号',
    `shape_type`          int(11)     NOT NULL COMMENT '资源团形状',
    `role_id`             bigint(20)  NOT NULL COMMENT '所属玩家id',
    `city_id`             bigint(20)  NOT NULL COMMENT '所属城池id',
    `mon_type`            int(11)     NOT NULL COMMENT '暗怪类型',
    PRIMARY KEY (`index`)
) ENGINE = MyISAM
  DEFAULT CHARSET = utf8mb4 COMMENT ='地图格子数据';

-- --------------------------------------------------------

--
-- 添加表，地图模板复活营寨、怪物表
-- lei
-- 2019092619
--

DROP TABLE IF EXISTS `db_tab_map_template`;
CREATE TABLE IF NOT EXISTS `db_tab_map_template`
(
    `pos`                 varchar(255) NOT NULL COMMENT '模板起始坐标',
    `surround_id`         int(11)      NOT NULL COMMENT 'map_surround的id',
    `config_city_list`    mediumblob COMMENT '小型营寨生成点列表',
    `config_monster_list` mediumblob COMMENT '明怪生成点列表',
    `now_city_num_list`   mediumblob COMMENT '当前需要的营寨数量列表',
    `now_mon_num_list`    mediumblob COMMENT '当前需要的怪物数量列表',
    PRIMARY KEY (`pos`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='模板表';

-- --------------------------------------------------------

--
-- 添加表，地图屯田练兵数据
-- zsk
-- 2018091701
--

DROP TABLE IF EXISTS `db_tab_map_train`;
CREATE TABLE IF NOT EXISTS `db_tab_map_train`
(
    `index`            int(11)    NOT NULL COMMENT '序号',
    `train_army`       bigint(20) NOT NULL COMMENT '屯田练兵军队',
    `train_type`       int(11)    NOT NULL COMMENT '屯田练兵类型',
    `train_num`        int(11)    NOT NULL COMMENT '屯田练兵次数',
    `train_start_time` int(11)    NOT NULL COMMENT '屯田练兵开始时间',
    PRIMARY KEY (`index`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='地图屯田练兵数据';

-- --------------------------------------------------------

--
-- 添加表，赛季账号表
-- lin
-- 2017091301
--

DROP TABLE IF EXISTS `db_tab_match_account`;
CREATE TABLE IF NOT EXISTS `db_tab_match_account`
(
    `acc_name`            varchar(64) NOT NULL COMMENT '账号名',
    `pass_word`           mediumblob COMMENT '密码',
    `role_id_list`        mediumblob COMMENT '未回归的角色id列表',
    `return_role_id_list` mediumblob COMMENT '已回归的角色id列表',
    PRIMARY KEY (`acc_name`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='赛季角色表';

-- --------------------------------------------------------

--
-- 添加表，赛季奖励表
-- lin
-- 20190819
--

DROP TABLE IF EXISTS `db_tab_match_reward`;
CREATE TABLE IF NOT EXISTS `db_tab_match_reward`
(
    `role_id`            bigint(20) NOT NULL COMMENT '角色id',
    `settle_season_id`   int(11)    NOT NULL COMMENT '第几赛季结算的',
    `send_season_id`     int(11)    NOT NULL COMMENT '第几赛季发放',
    `season_reward_list` mediumblob COMMENT '赛季奖励列表：[#r_season_reward{}, ...]',
    `season_summary`     mediumblob COMMENT '赛季弹窗内容',
    `mail_param_list`    mediumblob COMMENT '赛季奖励邮件数据[#r_season_reward_mail{}]',
    PRIMARY KEY (`role_id`),
    KEY `settle_season_id` (`settle_season_id`) COMMENT '结算奖励赛季索引'
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='赛季角色表';

-- --------------------------------------------------------

--
-- 添加表，赛季角色表（上个赛季需要保存的角色相关数据都放这里）
-- lin
-- 2017091401
--

DROP TABLE IF EXISTS `db_tab_match_role`;
CREATE TABLE IF NOT EXISTS `db_tab_match_role`
(
    `role_id`           bigint(20)  NOT NULL COMMENT '角色id',
    `acc_name`          varchar(64) NOT NULL COMMENT '账号名',
    `name`              varchar(20) NOT NULL COMMENT '角色名',
    `sex`               tinyint(4)  NOT NULL COMMENT '性别',
    `general_list`      mediumblob COMMENT '赛季保留的武将信息列表。[#r_season_general{}, ...]',
    `vip_info`          mediumblob COMMENT '赛季保留玩家VIP信息。[#r_season_vip{}, ...]',
    `skill_bag_list`    mediumblob COMMENT '赛季保留的技能背包信息列表。[#r_season_skill_bag{}, ...]',
    `card`              mediumblob COMMENT '月卡信息：#r_season_card{}',
    `resource_goods`    mediumblob COMMENT '赛季保留的资源物品列表。[#r_season_goods{}, ...]',
    `is_return`         smallint(6) NOT NULL COMMENT '本赛季是否已经回归',
    `last_server_num`   int(11)     NOT NULL COMMENT '上赛季的服务器编号',
    `first_server_num`  int(11)     NOT NULL COMMENT '第一赛季服务器编号',
    `role_code`         mediumblob COMMENT '玩家邀请码',
    `activity_data`     mediumblob COMMENT '活动数据',
    `head`              mediumblob COMMENT '头像、头像框',
    `city_skin`         mediumblob COMMENT '城池皮肤',
    `mini_card`         mediumblob COMMENT '小月卡信息：#r_season_card{}',
    `mystery_card`      mediumblob COMMENT '神秘卡',
    `gift_code`         mediumblob COMMENT '礼包码',
    `common_activities` mediumblob COMMENT '通用跨赛季活动',
    `season_history`    mediumblob COMMENT '赛季历史数据',
    `general_book`      mediumblob COMMENT '武将图鉴',
    `recruit`           mediumblob COMMENT '赛季历史数据',
    `qq_reward`         blob COMMENT 'QQ奖励数据：#r_season_card{}',
    `solder_skin`       mediumblob COMMENT '兵种皮肤',
    PRIMARY KEY (`role_id`),
    KEY `is_return` (`is_return`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='赛季角色表';

-- --------------------------------------------------------

--
-- 添加表，阅历经验
-- lei
-- 2019122719
--

DROP TABLE IF EXISTS `db_tab_offline_exp`;
CREATE TABLE IF NOT EXISTS `db_tab_offline_exp`
(
    `role_id`           bigint(20)       NOT NULL COMMENT '角色id',
    `offline_exp`       bigint(20)                DEFAULT NULL COMMENT '离线经验',
    `last_add_time`     int(11)                   DEFAULT NULL COMMENT '上次加时间',
    `addition_speed`    int(11)          NOT NULL DEFAULT '0' COMMENT '额外增速',
    `addition_etime`    int(11) UNSIGNED NOT NULL DEFAULT '0' COMMENT '额外增速结束时间',
    `addition_duration` int(11)          NOT NULL DEFAULT '0' COMMENT '持续时间，单位小时',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='阅历经验';

-- --------------------------------------------------------

--
-- 添加表，角色储备信息
-- daer
-- 2017080206
--

DROP TABLE IF EXISTS `db_tab_res_store`;
CREATE TABLE IF NOT EXISTS `db_tab_res_store`
(
    `role_id`         bigint(20) NOT NULL COMMENT '角色id',
    `free_num`        int(11)    NOT NULL COMMENT '使用的免费次数',
    `force_num`       int(11)    NOT NULL COMMENT '使用的强征次数',
    `next_store_time` int(11)    NOT NULL COMMENT '下次储备时间',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='角色储备信息';

-- --------------------------------------------------------

--
-- 添加表，角色表
-- daer
-- 2017080401
--

DROP TABLE IF EXISTS `db_tab_role`;
CREATE TABLE IF NOT EXISTS `db_tab_role`
(
    `role_id`                 bigint(20)          NOT NULL COMMENT '角色id',
    `acc_name`                varchar(64)         NOT NULL COMMENT '账号名',
    `channel`                 varchar(20)         NOT NULL COMMENT '渠道',
    `name`                    varchar(20)         NOT NULL COMMENT '角色名',
    `country`                 int(11)             NOT NULL COMMENT '国家',
    `sex`                     tinyint(4)          NOT NULL COMMENT '性别',
    `city_id_list`            mediumblob COMMENT '城池列表',
    `grid_index_sets`         mediumblob COMMENT '属于自己的格子集合',
    `creat_time`              bigint(20)          NOT NULL COMMENT '角色创建时间',
    `role_login_time`         bigint(20)          NOT NULL COMMENT '角色进程上次登录时间',
    `role_off_line_time`      bigint(20)          NOT NULL COMMENT '角色进程上次离线时间',
    `client_login_time`       bigint(20)          NOT NULL COMMENT '客户端上次登录时间',
    `client_offline_time`     bigint(20)          NOT NULL COMMENT '客户端上次关闭时间',
    `lev`                     int(11)             NOT NULL COMMENT '等级',
    `exp`                     int(11)             NOT NULL COMMENT '经验',
    `chat_info`               longblob COMMENT '聊天重置信息',
    `shop_info`               mediumblob COMMENT '商店信息',
    `recharge_info`           mediumblob COMMENT '充值信息',
    `fighting`                bigint(20)                   DEFAULT NULL COMMENT '战力',
    `is_in_novice`            tinyint(4)          NOT NULL COMMENT '是否处于新手引导阶段',
    `head`                    mediumblob COMMENT '头像、头像框',
    `personal_statement`      mediumblob COMMENT '个人说明',
    `change_name_num`         int(11)                      DEFAULT NULL COMMENT '改名次数',
    `main_city_id`            bigint(20)          NOT NULL COMMENT '玩家主城id',
    `skill_bag`               mediumblob COMMENT '武将技能背包',
    `novice_recruit_num`      tinyint(4)          NOT NULL COMMENT '新手引导已招募次数',
    `day_online_time`         int(11)                      DEFAULT NULL COMMENT '当天在线时长',
    `total_online_time`       int(11)                      DEFAULT NULL COMMENT '总在线时长',
    `policy_num`              int(11)                      DEFAULT NULL COMMENT '发布政策次数',
    `res_store_info`          mediumblob COMMENT '角色储备信息',
    `device_type`             int(11)             NOT NULL COMMENT '客户端类型，1为ios, 2为安卓',
    `is_first_login`          smallint(6)         NOT NULL COMMENT '是否是新号创角第一次登录',
    `is_random_country`       smallint(6)         NOT NULL COMMENT '是否是随机选择国家',
    `last_refresh_day`        mediumblob COMMENT '上次刷新的时间',
    `grid_lvl_list`           mediumblob COMMENT '占领过的地块等级列表',
    `celebrities_system_data` mediumblob COMMENT '名士系统数据',
    `good_use_time_list`      mediumblob COMMENT '使用物品小类时间戳列表',
    `is_swap_main`            smallint(6)         NOT NULL COMMENT '主城是否迁移过',
    `last_server_num`         int(11)             NOT NULL COMMENT '上赛季的服务器编号',
    `first_server_num`        int(11)             NOT NULL COMMENT '第一赛季服务器',
    `is_city_created`         tinyint(3) UNSIGNED NOT NULL DEFAULT '0' COMMENT '是否曾经创建分城',
    `is_active`               tinyint(3) UNSIGNED NOT NULL DEFAULT '1' COMMENT '是否活跃数据',
    `is_paid`                 tinyint(3) UNSIGNED NOT NULL DEFAULT '0' COMMENT '是否曾经付费',
    `patrol_grid_log`         mediumblob COMMENT '巡查搜索地点日志',
    `role_code`               mediumblob COMMENT '玩家邀请码',
    `city_skin`               mediumblob COMMENT '城池皮肤',
    `mail_flag`               mediumblob COMMENT '邮件设置',
    `official_position`       int(11)                      DEFAULT '0' COMMENT '个人官爵',
    `is_got_daily_salary`     tinyint(4)                   DEFAULT '0' COMMENT '是否已领取个人俸禄, 0否1是',
    `policy_cd`               mediumblob          NOT NULL COMMENT '政策更改cd',
    `fight_honor_count`       mediumblob          NOT NULL COMMENT '战功统计',
    `voice_id`                int(11)             NOT NULL DEFAULT '0' COMMENT '语音id',
    `chat_report`             mediumblob COMMENT '聊天举报',
    `is_union_open`           int(11)             NOT NULL COMMENT '聊天举报',
    `rebate_data`             mediumblob COMMENT '返利数据',
    `top_power`               int(10) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '历史最大势力值',
    `device_name`             varchar(256)        NOT NULL DEFAULT '0' COMMENT '设备名称',
    `app_version`             varchar(256)        NOT NULL DEFAULT '0' COMMENT '应用版本',
    `res_version`             varchar(256)        NOT NULL DEFAULT '0' COMMENT '资源版本',
    `solder_skin`             mediumblob COMMENT '兵种皮肤',
    `save_values`             longblob            NOT NULL COMMENT '需要永久保存的杂项数据',
    `destroy_citys`           longblob            NOT NULL COMMENT '被摧毁城池信息，[#r_destroy_city{}]',
    `cost_gold`               bigint(20) UNSIGNED NOT NULL DEFAULT '0' COMMENT '历史花费金元宝数量',
    `temp_cost`               mediumblob          NOT NULL COMMENT '临时扣除的资源列表，[{subject_type,consume_list}]',
    `daily_mon_tide`          int(11)             NOT NULL DEFAULT '0' COMMENT '每日尸潮次数',
    `monster_tide`            mediumblob COMMENT '尸潮',
    `radar_sign`              mediumblob COMMENT '查探标记',
    `radar_log`               mediumblob COMMENT '雷达日志',
    `last_sign_time`          bigint(20) UNSIGNED NOT NULL DEFAULT '0' COMMENT '最后一次发送标记时间',
    PRIMARY KEY (`role_id`),
    KEY `name` (`name`) COMMENT '角色名索引'
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='角色';

-- --------------------------------------------------------

--
-- 添加表，活动表
-- lei
-- 2017090401
--

DROP TABLE IF EXISTS `db_tab_role_activity`;
CREATE TABLE IF NOT EXISTS `db_tab_role_activity`
(
    `role_id`       bigint(20) NOT NULL COMMENT '角色id',
    `activity_list` mediumblob COMMENT '角色开启的活动列表',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='活动表';

-- --------------------------------------------------------

--
-- 添加表，玩家邀请好友无视封区限制活动
-- djc
-- 2020091014
--

DROP TABLE IF EXISTS `db_tab_role_act_mutual`;
CREATE TABLE IF NOT EXISTS `db_tab_role_act_mutual`
(
    `role_id`     bigint(20) NOT NULL COMMENT '角色id',
    `inv_info`    mediumblob COMMENT '邀请信息',
    `be_inv_info` mediumblob COMMENT '被邀请的信息',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='共济天下活动';

-- --------------------------------------------------------

--
-- 添加表，防沉迷表
-- lei
-- 2017090801
--

DROP TABLE IF EXISTS `db_tab_role_addiction`;
CREATE TABLE IF NOT EXISTS `db_tab_role_addiction`
(
    `role_id`            bigint(20) NOT NULL COMMENT '角色id',
    `real_name`          varchar(20) DEFAULT NULL COMMENT '真实姓名',
    `id_card`            varchar(20) DEFAULT NULL COMMENT '身份证',
    `status`             int(11)     DEFAULT NULL COMMENT '沉迷状态',
    `total_online_time`  int(11)     DEFAULT NULL COMMENT '今天累计在线时间',
    `total_offline_time` int(11)     DEFAULT NULL COMMENT '今天累计下线时间',
    `online_time`        int(11)     DEFAULT NULL COMMENT '上线时间戳',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='防沉迷表';

-- --------------------------------------------------------

--
-- 添加表，纪念卡池表
-- gjx
-- 2020102110
--

DROP TABLE IF EXISTS `db_tab_role_anniversary`;
CREATE TABLE IF NOT EXISTS `db_tab_role_anniversary`
(
    `id`               bigint(20)       NOT NULL COMMENT '纪念卡池ID（对应表AnniversaryRecruiting的主键）',
    `occupy_type`      blob             NOT NULL COMMENT '占领城池类型列表',
    `union_id`         bigint(20)                DEFAULT NULL,
    `state`            int(10) UNSIGNED NOT NULL DEFAULT '0' COMMENT '状态',
    `tick`             bigint(20)       NOT NULL COMMENT '阶段时间戳',
    `voter`            mediumblob COMMENT '投票玩家角色ID列表',
    `pool`             mediumblob COMMENT '卡池信息',
    `freer`            mediumblob COMMENT '首占世族成员 角色ID列表',
    `recruit_end_time` bigint(20)       NOT NULL COMMENT '纪念卡池招募入口结束时间戳',
    `union_name`       varchar(20)      NOT NULL COMMENT '联盟名称',
    `country_id`       int(10) UNSIGNED NOT NULL DEFAULT '0' COMMENT '占领国家ID',
    `city_name`        int(11)          NOT NULL DEFAULT '0' COMMENT '城池名字',
    `send_list`        mediumblob COMMENT '首占世族成员 已发送首占信息的玩家ID列表',
    PRIMARY KEY (`id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='纪念卡池表';

-- --------------------------------------------------------

--
-- 添加表，角色竞技场表
-- daer
-- 2021070705
--

DROP TABLE IF EXISTS `db_tab_role_arena`;
CREATE TABLE IF NOT EXISTS `db_tab_role_arena`
(
    `role_id`        bigint(20) UNSIGNED NOT NULL COMMENT '角色id',
    `win_num`        int(11) UNSIGNED    NOT NULL COMMENT '胜场',
    `battle_num`     int(11) UNSIGNED    NOT NULL COMMENT '总战斗场次',
    `point`          int(11) UNSIGNED    NOT NULL COMMENT '当前累积积分，如果在淘汰赛后首次获得积分需先清零',
    `confirm_expire` tinyint(1) UNSIGNED NOT NULL COMMENT '友谊赛邀请超时是否需要确认，0否，1需要',
    `c_time`         int(11) UNSIGNED    NOT NULL COMMENT '获得积分时间戳',
    `arena_task`     mediumblob COMMENT '竞技场任务相关数据，#r_arena_task{}',
    `record_list`    mediumblob COMMENT '竞技场战斗记录列表，[#r_arena_record{}|_]',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='角色竞技场表';

-- --------------------------------------------------------

--
-- 添加表，军队表
-- daer
-- 2017080601
--

DROP TABLE IF EXISTS `db_tab_role_army`;
CREATE TABLE IF NOT EXISTS `db_tab_role_army`
(
    `army_id`               bigint(20) NOT NULL COMMENT '军队id',
    `camp_id`               bigint(20) NOT NULL COMMENT '营寨id',
    `role_id`               bigint(20) NOT NULL COMMENT '角色id',
    `leader_id`             bigint(20) NOT NULL COMMENT '领队id',
    `battle_cells`          mediumblob COMMENT '武将信息',
    `extend_args`           mediumblob COMMENT '扩展信息',
    `cost_skill`            mediumblob COMMENT '军队统帅技能信息',
    `transport_building_id` bigint(20) NOT NULL COMMENT '军队执行运输事件的npc城池大本营id',
    PRIMARY KEY (`army_id`),
    KEY `role_id` (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='角色军队';

-- --------------------------------------------------------

--
-- 添加表，玩家打过地块的历史次数记录表
-- hyq
-- 2019102701
--

DROP TABLE IF EXISTS `db_tab_role_attacked_grid`;
CREATE TABLE IF NOT EXISTS `db_tab_role_attacked_grid`
(
    `role_id`            bigint(20) NOT NULL COMMENT '角色id',
    `attacked_grid_list` mediumblob COMMENT '当前需要的怪物数量列表',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='玩家地块历史攻击次数记录表';

-- --------------------------------------------------------

--
-- 添加表，聊天黑名单表
-- lei
-- 2019070114
--

DROP TABLE IF EXISTS `db_tab_role_ban`;
CREATE TABLE IF NOT EXISTS `db_tab_role_ban`
(
    `role_id`  bigint(20) NOT NULL COMMENT '角色id',
    `ban_list` mediumblob COMMENT '黑名单列表',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='聊天黑名单表';

-- --------------------------------------------------------

--
-- 添加表，建筑表
-- daer
-- 2017080901
--

DROP TABLE IF EXISTS `db_tab_role_building`;
CREATE TABLE IF NOT EXISTS `db_tab_role_building`
(
    `building_id` bigint(20) NOT NULL COMMENT '建筑id',
    `role_id`     bigint(20) NOT NULL COMMENT '角色id',
    `extend_args` mediumblob COMMENT '建筑功能record',
    PRIMARY KEY (`building_id`),
    KEY `role_id` (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='建筑';

-- --------------------------------------------------------

--
-- 添加表，角色军营技能
-- daer
-- 2021070706
--

DROP TABLE IF EXISTS `db_tab_role_camp_skill`;
CREATE TABLE IF NOT EXISTS `db_tab_role_camp_skill`
(
    `role_id` bigint(20) NOT NULL COMMENT '角色id',
    `cs_list` mediumblob COMMENT '城池军营技能数据 [#r_city_camp_skill{}]',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='角色军营技能';

-- --------------------------------------------------------

--
-- 添加表，神秘卡
-- hyq
-- 2020031802
--

DROP TABLE IF EXISTS `db_tab_role_card`;
CREATE TABLE IF NOT EXISTS `db_tab_role_card`
(
    `role_id`           bigint(20) NOT NULL COMMENT '角色id',
    `buy_times`         int(11)             DEFAULT NULL COMMENT '印玺每日购买次数',
    `card_list`         mediumblob NOT NULL COMMENT '神秘卡列表',
    `seal_use_list`     mediumblob COMMENT '个人印玺使用记录',
    `power_charge_list` mediumblob COMMENT '个人能量补充记录',
    `reset_cd`          int(11)    NOT NULL DEFAULT '0' COMMENT '武将还原冷却CD',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='神秘卡';

-- --------------------------------------------------------

--
-- 添加表，角色名士表
-- zsk
-- 2017091001
--

DROP TABLE IF EXISTS `db_tab_role_celebrities`;
CREATE TABLE IF NOT EXISTS `db_tab_role_celebrities`
(
    `celebrities_id`   bigint(20)  NOT NULL COMMENT '名士唯一id',
    `role_id`          bigint(20)  NOT NULL COMMENT '角色id',
    `type`             int(11)     NOT NULL COMMENT '名士类型（Celebrities表id）',
    `state`            int(11)     NOT NULL COMMENT '名士状态',
    `city_id`          bigint(20)  NOT NULL COMMENT '所在城池id（上任或者游学）',
    `city_office`      int(11)     NOT NULL COMMENT '上任的官职',
    `study_type`       int(11)     NOT NULL COMMENT '游学类型',
    `study_start_time` int(11)     NOT NULL COMMENT '游学开始时间',
    `study_end_time`   int(11)     NOT NULL COMMENT '游学结束时间',
    `study_reward`     mediumblob  NOT NULL COMMENT '游学奖励',
    `level`            int(11)     NOT NULL COMMENT '名士等级',
    `exp`              int(11)     NOT NULL COMMENT '名士经验',
    `is_hang_up`       smallint(6) NOT NULL COMMENT '是否挂机中',
    `hang_building_id` bigint(20)  NOT NULL COMMENT '挂机的建筑id',
    `merge_num`        int(11)     NOT NULL COMMENT '名士升级消耗的其他名士数量',
    `cost_skill`       mediumblob COMMENT '统帅技信息',
    PRIMARY KEY (`celebrities_id`),
    KEY `role_id` (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4;

-- --------------------------------------------------------

--
-- 添加表，角色信息
-- daer
-- 2017080205
--

DROP TABLE IF EXISTS `db_tab_role_chat`;
CREATE TABLE IF NOT EXISTS `db_tab_role_chat`
(
    `role_id`             bigint(20) NOT NULL COMMENT '角色id',
    `global_last_reset`   int(11)    NOT NULL COMMENT '世界次数上次重置时间',
    `country_last_reset`  int(11)    NOT NULL COMMENT '国家次数上次重置时间',
    `global_num`          int(11)    NOT NULL COMMENT '世界聊天次数',
    `country_num`         int(11)    NOT NULL COMMENT '国家聊天次数',
    `global_interval`     int(11)    NOT NULL COMMENT '世界聊天间隔',
    `country_interval`    int(11)    NOT NULL COMMENT '国家聊天间隔',
    `union_interval`      int(11)    NOT NULL COMMENT '联盟聊天间隔',
    `person_interval`     int(11)    NOT NULL COMMENT '个人聊天间隔',
    `person_chat_list`    longblob COMMENT '个人聊天列表',
    `union_team_interval` int(11)    NOT NULL DEFAULT '0' COMMENT '联盟小队聊天间隔',
    `group_interval`      int(11)    NOT NULL DEFAULT '0' COMMENT '群聊聊天间隔',
    `group_chat_list`     mediumblob COMMENT '群聊id列表',
    `chat_set`            mediumblob COMMENT '群聊设置',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='角色信息';

-- --------------------------------------------------------

--
-- 添加表，角色城池任务表
-- lei
-- 2018111301
--

DROP TABLE IF EXISTS `db_tab_role_city_task`;
CREATE TABLE IF NOT EXISTS `db_tab_role_city_task`
(
    `role_id`               bigint(20) NOT NULL COMMENT '角色id',
    `village_link_list`     mediumblob COMMENT '村庄联系列表',
    `cele_move_list`        mediumblob COMMENT '赠送名士走路列表',
    `point_list`            longblob COMMENT '建筑或城池的路列表',
    `history_link_tid_list` mediumblob COMMENT '历史联系的村庄类型列表',
    `history_link_id_list`  mediumblob COMMENT '历史联系村庄id列表',
    `finish_num`            int(11) DEFAULT NULL COMMENT '当天完成任务数',
    `accept_num`            int(11) DEFAULT NULL COMMENT '已领取任务数',
    `max_finish_num`        int(11) DEFAULT NULL COMMENT '最大完成任务数',
    `paving_road_list`      mediumblob COMMENT '正在铺设的道路',
    `cele_preach_list`      blob COMMENT '到达村庄后游学（传道）的名士',
    `delegate_objects`      mediumblob COMMENT '委托类对象',
    `messenger_move_list`   mediumblob COMMENT '信使移动列表',
    `smith_move_list`       mediumblob COMMENT '任务的工匠移动列表',
    `reward_village_list`   blob COMMENT '领取奖励的村庄id',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='角色城池任务表';

-- --------------------------------------------------------

--
-- 新增表，自主创建的怪物
-- xiewendong
-- 2020122810
-- 

DROP TABLE IF EXISTS `db_tab_role_create_mon`;
CREATE TABLE IF NOT EXISTS `db_tab_role_create_mon`
(
    `army_id`     bigint(20) UNSIGNED NOT NULL DEFAULT '0' COMMENT '领队的军队id，同时也是展示的军队id',
    `role_id`     bigint(20) UNSIGNED NOT NULL DEFAULT '0' COMMENT '谁创建的',
    `pos_x`       int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '创建时的坐标X',
    `pos_y`       int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '创建时的坐标Y',
    `block_flag`  int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '创建时添加的阻挡类型',
    `cfg_id`      int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '配置表的id（目前是activity_snow_man）',
    `activity_id` int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '活动id，关闭活动的时候需要删除',
    `is_enable`   tinyint(3) UNSIGNED NOT NULL DEFAULT '0' COMMENT '是否启用',
    PRIMARY KEY (`army_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='玩家创建怪物表';

-- --------------------------------------------------------

--
-- 添加表，防刷验证表
-- hyq
-- 2020110622
--

DROP TABLE IF EXISTS `db_tab_role_defend_cheat`;
CREATE TABLE IF NOT EXISTS `db_tab_role_defend_cheat`
(
    `role_id`            bigint(20) NOT NULL COMMENT '角色id',
    `client_online_long` int(11) DEFAULT NULL COMMENT '自上一次验证后，到现在的客户端在线时长',
    `verify_result`      int(11)    NOT NULL COMMENT '验证结果',
    `verify_data`        blob       NOT NULL COMMENT '验证数据',
    `client_login_time`  bigint(20) NOT NULL COMMENT '客户端登录时间戳',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='防刷验证表';

-- --------------------------------------------------------

--
-- 添加表，外交表
-- lei
-- 2017090101
--

DROP TABLE IF EXISTS `db_tab_role_diplomacy`;
CREATE TABLE IF NOT EXISTS `db_tab_role_diplomacy`
(
    `role_id`              bigint(20) NOT NULL COMMENT '角色id',
    `diplomacy_list`       mediumblob COMMENT '角色外交列表',
    `diplomacy_event_list` mediumblob COMMENT '外交事件列表',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='外交表';

-- --------------------------------------------------------

--
-- 添加表，六爻起卦表
-- daer
-- 2021070707
--

DROP TABLE IF EXISTS `db_tab_role_divination`;
CREATE TABLE IF NOT EXISTS `db_tab_role_divination`
(
    `role_id`     bigint(20) NOT NULL COMMENT '角色id',
    `attach_time` int(11)    NOT NULL COMMENT '六爻起卦触发时间',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='六爻起卦表';

-- --------------------------------------------------------

--
-- 添加表，武将征兵折扣数据
-- hyq
-- 2020121601
--

DROP TABLE IF EXISTS `db_tab_role_draft_discount`;
CREATE TABLE IF NOT EXISTS `db_tab_role_draft_discount`
(
    `role_id`       bigint(20) NOT NULL COMMENT '角色id',
    `discount_list` blob COMMENT '武将征兵折扣列表',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='武将征兵折扣数据';

-- --------------------------------------------------------

--
-- 添加表，怪物掉落表
-- hyq
-- 2019070401
--

DROP TABLE IF EXISTS `db_tab_role_drop_data`;
CREATE TABLE IF NOT EXISTS `db_tab_role_drop_data`
(
    `role_id`    bigint(20) NOT NULL COMMENT '角色id',
    `timer_ref`  int(11)    NOT NULL COMMENT '掉落列表自动进背包的定时器的ref',
    `drops_list` mediumblob COMMENT '怪物掉落表',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='怪物掉落表';

-- --------------------------------------------------------

--
-- 添加表，事件日志
-- lin
-- 2017080501
--

DROP TABLE IF EXISTS `db_tab_role_event`;
CREATE TABLE IF NOT EXISTS `db_tab_role_event`
(
    `role_id`             bigint(20) NOT NULL COMMENT '角色id',
    `acc_id`              bigint(20) NOT NULL COMMENT '事件累计id',
    `army_event_list`     mediumblob COMMENT '部队事件列表 [#r_event_log{}, ...]',
    `city_event_list`     mediumblob COMMENT '城池事件列表 [#r_event_log{}, ...]',
    `warning_event_list`  mediumblob COMMENT '预警事件列表 [#r_event_log{}, ...]',
    `study_event_list`    mediumblob COMMENT '名士游学事件列表 [#r_event_log{}, ...]',
    `county_event_list`   mediumblob COMMENT '县城事件列表 [#r_event_log{}, ...]',
    `scrap_event_list`    mediumblob COMMENT '废墟城池繁荣度相关 [#r_event_log{}, ...]',
    `warrior_event_list`  mediumblob COMMENT '斥候事件列表 [#r_event_log{}, ...]',
    `common_event_list`   mediumblob COMMENT '普通事件 ring_buffer(#r_event_log{})',
    `emergent_event_list` mediumblob COMMENT '紧急事件 ring_buffer(#r_event_log{})',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='事件日志';

-- --------------------------------------------------------

--
-- 添加表，角色功能表
-- lei
-- 2017090501
--

DROP TABLE IF EXISTS `db_tab_role_function`;
CREATE TABLE IF NOT EXISTS `db_tab_role_function`
(
    `role_id`       bigint(20) NOT NULL COMMENT '角色id',
    `function_list` mediumblob COMMENT '角色开启的功能列表',
    `gm_all_func`   int(11)    NOT NULL COMMENT 'gm开启功能',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='活动表';

-- --------------------------------------------------------

--
-- 添加表，武将表
-- zsk
-- 2017081401
--

DROP TABLE IF EXISTS `db_tab_role_general`;
CREATE TABLE IF NOT EXISTS `db_tab_role_general`
(
    `general_id`     bigint(20) NOT NULL COMMENT '武将ID',
    `role_id`        bigint(20) NOT NULL COMMENT '角色id',
    `general_type`   int(11)    NOT NULL COMMENT '武将类型',
    `general_lev`    int(11)    NOT NULL COMMENT '武将等级',
    `general_exp`    int(11)    NOT NULL COMMENT '武将经验',
    `general_skill`  mediumblob NOT NULL COMMENT '武将技能',
    `general_star`   int(11)    NOT NULL COMMENT '武将星级',
    `general_color`  int(11)    NOT NULL COMMENT '武将品质',
    `camp_id`        bigint(20) NOT NULL COMMENT '上阵营寨id',
    `camp_pos`       bigint(20) NOT NULL COMMENT '兵营中上阵位置',
    `status`         int(11)    NOT NULL COMMENT '武将状态',
    `patrol_city_id` bigint(20) NOT NULL COMMENT '大于0，正在巡查的城池id；否则没在巡查',
    `is_patroling`   int(11)    NOT NULL COMMENT '是否巡查中。1未开始；2巡查中；3巡查结束',
    `fatigue`        int(11)    NOT NULL COMMENT '疲劳值',
    `fatigue_time`   int(11)    NOT NULL COMMENT '上次恢复疲劳值时间',
    `locked`         int(11)    NOT NULL COMMENT '是否锁定',
    `hurt_time`      int(11)    NOT NULL COMMENT '重伤持续时间',
    `dying_time`     int(11)    NOT NULL COMMENT '濒临死亡持续时间',
    `eqp_list`       mediumblob NOT NULL COMMENT '武将装备',
    `general_awaken` mediumblob COMMENT '武将觉醒信息',
    `step`           int(11)    NOT NULL COMMENT '武将阶级',
    `point_list`     mediumblob COMMENT '加点列表',
    PRIMARY KEY (`general_id`),
    KEY `role_id` (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='武将表';

-- --------------------------------------------------------

--
-- 添加表，玩家礼包码信息
-- hyq
-- 2020072901
--

DROP TABLE IF EXISTS `db_tab_role_gift_code`;
CREATE TABLE IF NOT EXISTS `db_tab_role_gift_code`
(
    `role_id`           bigint(20) NOT NULL COMMENT '角色id',
    `gift_code_list`    mediumblob COMMENT '角色使用过的礼包码列表',
    `batch_number_list` mediumblob COMMENT '角色礼包批号信息',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='玩家礼包码信息';

-- --------------------------------------------------------

--
-- 添加表，物品表
-- daer
-- 2017081301
--

DROP TABLE IF EXISTS `db_tab_role_goods`;
CREATE TABLE IF NOT EXISTS `db_tab_role_goods`
(
    `goods_id`      bigint(20)  NOT NULL COMMENT '物品id',
    `role_id`       bigint(20)  NOT NULL COMMENT '角色id',
    `goods_type`    int(11)     NOT NULL COMMENT '物品类型',
    `goods_num`     int(11)     NOT NULL COMMENT '物品数量',
    `goods_quality` int(11)     NOT NULL COMMENT '物品品质',
    `bag_type`      smallint(6) NOT NULL DEFAULT '0' COMMENT '背包类型：0主背包1碎片背包',
    `overdue_time`  int(11)     NOT NULL COMMENT '过期时间',
    `general_id`    int(11)     NOT NULL COMMENT '穿戴该物品装备的武将id',
    `strong_lev`    int(11)     NOT NULL COMMENT '装备物品强化等级',
    `package_item`  mediumblob COMMENT '扩展字段：[{物品类型, 物品数量}, ...]',
    PRIMARY KEY (`goods_id`),
    KEY `role_id` (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='物品表';

-- --------------------------------------------------------

--
-- 添加表，邮件表
-- lei
-- 2017081501
--

DROP TABLE IF EXISTS `db_tab_role_mail`;
CREATE TABLE IF NOT EXISTS `db_tab_role_mail`
(
    `mail_id`         bigint(20) NOT NULL COMMENT '邮件id',
    `title`           mediumblob COMMENT '标题',
    `send_id`         bigint(20) NOT NULL COMMENT '发送者id',
    `send_acc`        mediumblob NOT NULL COMMENT '发送者名字',
    `receive_id`      bigint(20) NOT NULL COMMENT '接收者id',
    `receive_acc`     mediumblob NOT NULL COMMENT '接收者名字',
    `send_time`       int(11)    NOT NULL COMMENT '发送时间',
    `content`         mediumblob NOT NULL COMMENT '正文',
    `attachment`      mediumblob COMMENT '附件',
    `attachstatus`    tinyint(4) NOT NULL COMMENT '附件状态（0 未领取 1 已领）',
    `tab`             tinyint(4) NOT NULL COMMENT '页签（0 个人 1 联盟 2 国家 3 系统）',
    `collect`         tinyint(4) NOT NULL COMMENT '收藏（0 未收藏，1已收藏）',
    `status`          tinyint(4) NOT NULL COMMENT '状态（0 未读，1 已读）',
    `job`             tinyint(4) DEFAULT NULL COMMENT '职位',
    `alliance`        mediumblob COMMENT '联盟',
    `is_sys`          int(11)    NOT NULL COMMENT '是否系统发 0否 1是',
    `para_type`       int(11)    NOT NULL COMMENT '参数配置表id',
    `para_list`       mediumblob COMMENT '正文参数列表',
    `country_id`      int(11)    NOT NULL COMMENT '国家id',
    `mul_type`        int(11)    NOT NULL COMMENT '群发类型',
    `title_para_list` mediumblob COMMENT '正文参数列表',
    `is_proxy_leader` tinyint(4) DEFAULT '0' COMMENT '是否代理宗主,0否1是',
    `function_type`   tinyint(4) DEFAULT '0' COMMENT '系统类型（邮件来自哪个系统）',
    `extra`           mediumblob COMMENT '额外参数',
    PRIMARY KEY (`mail_id`),
    KEY `send_id` (`send_id`),
    KEY `receive_id` (`receive_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='邮件表';

-- --------------------------------------------------------

--
-- 新增表，赛季奖励概述
-- xiewendong
-- 2020100301
--

DROP TABLE IF EXISTS `db_tab_role_match_misc`;
CREATE TABLE IF NOT EXISTS `db_tab_role_match_misc`
(
    `role_id`        bigint(20)          NOT NULL COMMENT '角色id',
    `is_reward_read` tinyint(3) UNSIGNED NOT NULL DEFAULT '0' COMMENT '是否已经阅读了奖励',
    `reward_summary` mediumblob COMMENT '上个赛季的所有奖励',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='赛季数据杂项';

-- --------------------------------------------------------

--
-- 添加表，角色离线消息
-- xiewendong
-- 2021030101
--

DROP TABLE IF EXISTS `db_tab_role_msg_bus`;
CREATE TABLE IF NOT EXISTS `db_tab_role_msg_bus`
(
    `role_id`  bigint(20) NOT NULL COMMENT '角色id',
    `msg_list` mediumblob COMMENT '[{Id, {M, F, A}, AddTime}......]，逆序，最新的在前面',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='角色离线消息';

-- --------------------------------------------------------

--
-- 添加表，下线未执行函数表
-- lei
-- 2017090201
--

DROP TABLE IF EXISTS `db_tab_role_offline`;
CREATE TABLE IF NOT EXISTS `db_tab_role_offline`
(
    `role_id`  bigint(20) NOT NULL COMMENT '角色id',
    `mfa_list` mediumblob COMMENT 'MFA',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='下线未执行函数表';

-- --------------------------------------------------------

--
-- 添加表，充值表
-- lei
-- 2017082101
--

DROP TABLE IF EXISTS `db_tab_role_paypal`;
CREATE TABLE IF NOT EXISTS `db_tab_role_paypal`
(
    `order_id`        varchar(255) NOT NULL COMMENT '平台订单id',
    `role_id`         bigint(20)   NOT NULL COMMENT '角色id',
    `money`           varchar(255) NOT NULL COMMENT '软妹币-折扣后',
    `add_time`        varchar(255) NOT NULL COMMENT '创建时间',
    `custom_order_id` varchar(255) NOT NULL COMMENT '商户订单号',
    `pay_type`        varchar(255) NOT NULL COMMENT '充值类型',
    `send_date`       varchar(255) NOT NULL COMMENT '发送时间',
    `custom_info`     mediumblob COMMENT '商户自定义信息,请求充值时传递给sdk的信息',
    `acc_name`        varchar(64)  NOT NULL COMMENT '账号名',
    `product_id`      int(11)      NOT NULL COMMENT '计费点配置id',
    `ori_money`       varchar(255) NOT NULL COMMENT '软妹币-原价',
    PRIMARY KEY (`order_id`),
    KEY `role_id` (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='充值表';

-- --------------------------------------------------------

--
-- 添加表，付费保底表
-- hyq
-- 2020101503
--

DROP TABLE IF EXISTS `db_tab_role_pay_guarantee`;
CREATE TABLE IF NOT EXISTS `db_tab_role_pay_guarantee`
(
    `role_id`        bigint(20)       NOT NULL COMMENT '角色id',
    `pay_tid`        int(10) UNSIGNED NOT NULL DEFAULT '0' COMMENT '当前保底档位，RecruitingRecharge配置表的id',
    `cost`           int(10) UNSIGNED NOT NULL DEFAULT '0' COMMENT '招募累计花费的元宝数量(不区分金元宝和银元宝)',
    `finish_list`    blob             NOT NULL COMMENT '已轮询完的保底档位',
    `guarantee_list` blob             NOT NULL COMMENT '保底数据',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='招募-付费保底表';

-- --------------------------------------------------------

--
-- 新增表，个人势力值
-- dengjc
-- 2021020510
--

DROP TABLE IF EXISTS `db_tab_role_power`;
CREATE TABLE IF NOT EXISTS `db_tab_role_power`
(
    `role_id`            bigint(20) UNSIGNED NOT NULL DEFAULT '0' COMMENT '玩家id',
    `first_kill_mons`    mediumblob COMMENT '首次击杀盗匪-精英',
    `first_destory_camp` mediumblob COMMENT '首次摧毁营寨',
    `log`                mediumblob COMMENT '日志',
    `num`                int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '势力值',
    `refresh_time`       int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '刷新时间',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='个人势力值';

-- --------------------------------------------------------

--
-- 添加表，角色兵种职业表
-- lei
-- 2017090901
--

DROP TABLE IF EXISTS `db_tab_role_profession`;
CREATE TABLE IF NOT EXISTS `db_tab_role_profession`
(
    `role_id`              bigint(20) NOT NULL COMMENT '角色id',
    `city_profession_list` mediumblob COMMENT '城池职业列表',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='角色兵种职业表';

-- --------------------------------------------------------

--
-- 添加表，QQ特权表(蓝钻和QQ游戏大厅)
-- hyq
-- 2020120203
--

DROP TABLE IF EXISTS `db_tab_role_qq_private`;
CREATE TABLE IF NOT EXISTS `db_tab_role_qq_private`
(
    `role_id`         bigint(20) NOT NULL COMMENT '角色id',
    `qq_private_data` blob COMMENT '特权信息',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='QQ特权信息表';

-- --------------------------------------------------------

--
-- 添加表，充值信息
-- daer
-- 2017080208
--

DROP TABLE IF EXISTS `db_tab_role_recharge`;
CREATE TABLE IF NOT EXISTS `db_tab_role_recharge`
(
    `role_id`            bigint(20) NOT NULL COMMENT '角色id',
    `total_time`         int(11)    NOT NULL COMMENT '总充值次数',
    `diamond_time`       int(11)    NOT NULL COMMENT '充值元宝次数',
    `first_charge_state` int(11)    NOT NULL DEFAULT '0' COMMENT '首充状态',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='充值信息';

-- --------------------------------------------------------

--
-- 添加表，招募表
-- lin
-- 2017082001
--

DROP TABLE IF EXISTS `db_tab_role_recruit`;
CREATE TABLE IF NOT EXISTS `db_tab_role_recruit`
(
    `recruit_id`        bigint(20) NOT NULL COMMENT '招募id',
    `recruit_type`      int(11)    NOT NULL COMMENT '招募类型',
    `role_id`           bigint(20) NOT NULL COMMENT '角色id',
    `use_free_num`      int(11)    NOT NULL COMMENT '已使用免费次数',
    `use_half_num`      int(11)    NOT NULL COMMENT '已使用半价次数',
    `recruit_num`       bigint(20) NOT NULL COMMENT '历史已招募次数，永不清零（有次数上限的招募才会统计）',
    `history_num`       int(11)    NOT NULL COMMENT '本轮保底已招募次数，每轮结束清零',
    `last_free_time`    int(11)    NOT NULL COMMENT '上次刷新免费次数时间戳，只能钻石招募的招募类型这里为0',
    `merge_rct_list`    mediumblob NOT NULL COMMENT '合并的招募入口信息列表[#merge_rct{}]',
    `gold_rate`         int(11)    NOT NULL COMMENT '金卡保底权重加成百分比',
    `purple_rate`       int(11)    NOT NULL COMMENT '紫卡保底权重加成百分比',
    `end_time`          int(11)    NOT NULL COMMENT '招募入口消失时间戳。不会消失的入口这里为0',
    `season_free_limit` int(11)    NOT NULL COMMENT '赛季招募卡池剩余免费次数',
    `daily_recruit_num` bigint(20) DEFAULT '0' COMMENT '每日招募次数（零点清零）',
    `guarantee_list`    blob       NOT NULL COMMENT '特殊保底数据',
    `spec_data`         blob       NOT NULL COMMENT '精选招募数据',
    `extend`            mediumblob COMMENT '扩展字段',
    PRIMARY KEY (`recruit_id`),
    KEY `role_id` (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='招募系统';

-- --------------------------------------------------------

-- 添加表，抽卡概率表
-- daer
-- 2020111601
--

DROP TABLE IF EXISTS `db_tab_role_recruit_rate`;
CREATE TABLE IF NOT EXISTS `db_tab_role_recruit_rate`
(
    `key`    bigint(20) NOT NULL COMMENT 'key',
    `data`   longblob COMMENT 'data',
    `config` longblob COMMENT 'config',
    PRIMARY KEY (`key`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='抽卡概率表';

-- --------------------------------------------------------

--
-- 添加表，分城拆除缓存数据表
-- lin
-- 2019031101
--

DROP TABLE IF EXISTS `db_tab_role_remove`;
CREATE TABLE IF NOT EXISTS `db_tab_role_remove`
(
    `role_id`          bigint(20) NOT NULL COMMENT '玩家角色id',
    `remove_city_list` mediumblob COMMENT '分城被拆除后保存的信息列表。[#r_remove_city{}, ...]',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='分城拆除缓存数据';

-- --------------------------------------------------------

--
-- 添加表，玩家名士声望获取/减少途径表
-- lin
-- 2019090209
--

DROP TABLE IF EXISTS `db_tab_role_renown`;
CREATE TABLE IF NOT EXISTS `db_tab_role_renown`
(
    `role_id`          bigint(20) NOT NULL COMMENT '角色id',
    `behavior_list`    mediumblob COMMENT '获取/减少声望行为列表。[#r_renown_behavior{}, ...]',
    `type_list`        mediumblob COMMENT '同一类型的行为信息列表。[#r_behavior_type{}, ...]',
    `total_renown`     bigint(20) NOT NULL COMMENT '今日累计总声望（可能会是负值）',
    `total_add_renown` bigint(20) NOT NULL COMMENT '今日累计增加总声望',
    `total_sub_renown` bigint(20) NOT NULL COMMENT '今日累计减少总声望',
    `log_index`        bigint(20) NOT NULL COMMENT '日志序号，递增，同类型的行为增加一次，log_index + 1',
    `log_list`         mediumblob COMMENT '声望日志，每增加一个日志，按日志序号从大到小排序',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='联盟视野';

-- --------------------------------------------------------

--
-- 添加表，个人资源
-- daer
-- 2017081502
--

DROP TABLE IF EXISTS `db_tab_role_resource`;
CREATE TABLE IF NOT EXISTS `db_tab_role_resource`
(
    `role_id`             bigint(20) NOT NULL COMMENT '角色id',
    `last_add_time`       int(11)    NOT NULL COMMENT '上次添加的时间',
    `coin`                int(11)    NOT NULL COMMENT '金币',
    `diamond`             int(11)    NOT NULL COMMENT '元宝',
    `list`                mediumblob COMMENT '每种资源列表',
    `goods_speed_up_list` mediumblob COMMENT '物品加上列表',
    `donate`              int(11)    NOT NULL COMMENT '个人贡献值',
    `skill_exp`           int(11)    NOT NULL COMMENT '技能经验',
    `military`            mediumblob COMMENT '军令',
    `celebrities`         mediumblob COMMENT '名仕',
    `fight_honor`         int(11)    NOT NULL COMMENT '战功',
    `today_fight_honor`   int(11)    NOT NULL DEFAULT '0' COMMENT '今日战功-每日刷新',
    `is_push`             int(11)             DEFAULT NULL COMMENT '战功上限是否推送(0否1是)',
    `gong_xun`            int(11)    NOT NULL DEFAULT '0' COMMENT '功勋',
    `today_gong_xun`      int(11)    NOT NULL DEFAULT '0' COMMENT '今日功勋-每日刷新',
    `gold_diamond`        int(11)    NOT NULL COMMENT '金元宝',
    `speed_up_build`      int(11)    NOT NULL COMMENT '建筑加速槽',
    `speed_up_draft`      int(11)    NOT NULL COMMENT '征兵加速槽',
    `labour`              mediumblob COMMENT '体力',
    `warrior_pro`         mediumblob COMMENT '生产力',
    `tal_resource`        mediumblob COMMENT '国家天赋加的资源',
    `fight_honor_coin`    int(11)             DEFAULT NULL COMMENT '战功币',
    `snowball`            int(11)    NOT NULL DEFAULT '0' COMMENT '雪球',
    `snowflake`           mediumblob COMMENT '雪花',
    `power_blue`          int(11)    NOT NULL DEFAULT '0' COMMENT '蓝色能量',
    `power_red`           int(11)    NOT NULL DEFAULT '0' COMMENT '红色能量',
    `footprint`           int(11)    NOT NULL DEFAULT '0' COMMENT '年兽足迹',
    `power_red_bind`      int(11)    NOT NULL DEFAULT '0' COMMENT '红色能量(绑定)',
    `token_exp`           int(11)    NOT NULL DEFAULT '0' COMMENT '令牌经验',
    `purse`               int(11)    NOT NULL DEFAULT '0' COMMENT '钱袋',
    `compete_coin`        int(11)    NOT NULL DEFAULT '0' COMMENT '竞币',
    `tenon`               int(11)    NOT NULL DEFAULT '0' COMMENT '榫卯',
    `common_list`         mediumblob NOT NULL COMMENT '普通资源',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='个人资源';

-- --------------------------------------------------------

--
-- 添加表，玩家每日被掠夺资源
-- Karl
-- 2020072601
--

DROP TABLE IF EXISTS `db_tab_role_robed`;
CREATE TABLE IF NOT EXISTS `db_tab_role_robed`
(
    `role_id`     bigint(20) NOT NULL COMMENT '角色id',
    `daily_robed` mediumblob COMMENT '每日被抢信息',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='角色每日被抢信息';

-- --------------------------------------------------------

--
-- 添加表，新增历史赛季
-- xiewendong
-- 2020101501
--

DROP TABLE IF EXISTS `db_tab_role_season_history`;
CREATE TABLE IF NOT EXISTS `db_tab_role_season_history`
(
    `role_id`        bigint(20)          NOT NULL COMMENT '角色id',
    `fight_honor`    mediumblob COMMENT '赛季战功历史 [{赛季id，最高战功}...]',
    `gong_xun`       mediumblob COMMENT '功勋历史 [{赛季id, 最高功勋}]',
    `dating_role_id` bigint(20) UNSIGNED NOT NULL DEFAULT '0' COMMENT '大厅的角色id',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='角色的赛季历史数据';

-- --------------------------------------------------------

--
-- 添加表，赛季积分表
-- lei
-- 2017091501
--

DROP TABLE IF EXISTS `db_tab_role_season_score`;
CREATE TABLE IF NOT EXISTS `db_tab_role_season_score`
(
    `role_id`   bigint(20) NOT NULL COMMENT '角色id',
    `score`     int(11)    NOT NULL COMMENT '积分',
    `max_score` int(11)    NOT NULL COMMENT '最大积分',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='角色赛季积分表';

-- --------------------------------------------------------

--
-- 添加表，商店表
-- lei
-- 2017081801
--

DROP TABLE IF EXISTS `db_tab_role_shop`;
CREATE TABLE IF NOT EXISTS `db_tab_role_shop`
(
    `shop_id`           bigint(20) NOT NULL COMMENT '商店id',
    `role_id`           bigint(20) NOT NULL COMMENT '角色id',
    `shop_type`         tinyint(4) NOT NULL COMMENT '商店类型',
    `good_list`         mediumblob NOT NULL COMMENT '商店物品列表',
    `refresh_num`       int(11)    NOT NULL COMMENT '已使用刷新次数',
    `last_refresh_time` bigint(20) NOT NULL COMMENT '商店上次自动刷新时间',
    `last_reset_time`   bigint(20) NOT NULL COMMENT '上次重置刷新次数时间',
    PRIMARY KEY (`shop_id`),
    KEY `role_id` (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='商店表';

-- --------------------------------------------------------

--
-- 添加表，个人视野
-- daer
-- 2019071201
--

DROP TABLE IF EXISTS `db_tab_role_sight`;
CREATE TABLE IF NOT EXISTS `db_tab_role_sight`
(
    `role_id`               bigint(20) UNSIGNED NOT NULL DEFAULT '0' COMMENT '玩家id',
    `forever_sight_list`    mediumblob COMMENT '永久视野[Index,...]',
    `union_grid_sight_list` mediumblob COMMENT '联盟城池组地块视野[Index,...]',
    `road_sight_list`       mediumblob COMMENT '道路视野[{VillageId, [Index,...]},...]',
    `radar_sight_list`      mediumblob COMMENT '雷达视野[{key, time},...]',
    `other_sight_list`      mediumblob COMMENT '其他自定义视野[{key, val},...]',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='玩家个人相关视野';

-- --------------------------------------------------------

--
-- 添加表，个人标记表
-- lin
-- 2017081601
--

DROP TABLE IF EXISTS `db_tab_role_sign_data`;
CREATE TABLE IF NOT EXISTS `db_tab_role_sign_data`
(
    `role_id`   bigint(20) NOT NULL COMMENT '角色id',
    `sign_list` mediumblob COMMENT '个人标记列表。[#r_role_sign{}, ...]',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='个人标记';

-- --------------------------------------------------------

--
-- 添加表，士兵天赋（职业天赋？演武场天赋？）
-- Karl
-- 2020032601
--

DROP TABLE IF EXISTS `db_tab_role_soldier_talent`;
CREATE TABLE IF NOT EXISTS `db_tab_role_soldier_talent`
(
    `role_id`          bigint(20) NOT NULL DEFAULT '0' COMMENT '角色id',
    `city_talent_list` mediumblob COMMENT '天赋数据',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='演武场天赋';

-- --------------------------------------------------------

--
-- 添加表，任务表
-- zsk
-- 2017081701
--

DROP TABLE IF EXISTS `db_tab_role_task`;
CREATE TABLE IF NOT EXISTS `db_tab_role_task`
(
    `task_id`       bigint(20) NOT NULL COMMENT '任务ID',
    `role_id`       bigint(20) NOT NULL COMMENT '角色id',
    `task_sort`     int(11)    NOT NULL COMMENT '任务类型(大类)',
    `task_configid` int(11)    NOT NULL COMMENT '任务类型(小类)',
    `state`         int(11)    NOT NULL COMMENT '任务状态',
    `count`         bigint(20) NOT NULL COMMENT '计数器',
    `times`         int(11)    NOT NULL COMMENT '可完成次数(目前只有对活动任务生效)',
    `misc`          mediumblob COMMENT '扩展字段',
    `extra_data`    mediumblob COMMENT '扩展字段',
    PRIMARY KEY (`task_id`),
    KEY `role_id` (`role_id`),
    KEY `task_sort` (`task_sort`),
    KEY `task_configid` (`task_configid`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4;

-- --------------------------------------------------------

--
-- 新增表，天子令
-- gjx
-- 2021042701
--

DROP TABLE IF EXISTS `db_tab_role_token`;
CREATE TABLE IF NOT EXISTS `db_tab_role_token`
(
    `role_id`        bigint(20) UNSIGNED NOT NULL DEFAULT '0' COMMENT '玩家id',
    `exp`            int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '当前经验值',
    `lv`             int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '当前等级',
    `normal_reward`  blob COMMENT '已领取奖励（天子令）（等级列表）',
    `emperor_reward` blob COMMENT '已领取奖励（九霄天子令）（等级列表）',
    `buy_exp_time`   int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '当天已购买经验次数',
    `emperor`        int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '是否已解锁高级天子令',
    `nine_emperor`   int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '是否已解锁高级天子令+直升',
    `straight_up`    int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '是否已购买过直升产品',
    `shop_buy_list`  mediumblob COMMENT '本赛季已兑换的商品跟次数[{GoodsId,Times}|_]',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='天子令';

-- --------------------------------------------------------

--
-- 添加表，角色联盟信息表
-- lin
-- 2017082201
--

DROP TABLE IF EXISTS `db_tab_role_union`;
CREATE TABLE IF NOT EXISTS `db_tab_role_union`
(
    `role_id`           bigint(20) NOT NULL COMMENT '角色id',
    `apply_union_time`  int(11)    NOT NULL COMMENT '上次申请加入联盟时间戳',
    `buy_times_list`    mediumblob COMMENT '个人官爵对应的印玺购买次数列表',
    `power_gain_tuple`  mediumblob COMMENT '每日获得能量记录，{蓝色能量,红色能量}',
    `access_gain_tuple` mediumblob COMMENT '蓝色能量每天通过各种途径获得记录，{讨伐盗匪,讨伐营寨,完成村庄任务,攻打联盟城池,组队}',
    `other_charge_num`  mediumblob COMMENT '为他人总充能量，{蓝色能量,红色能量}',
    `self_charge_num`   mediumblob COMMENT '为自己总充能量，{蓝色能量,红色能量}',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='角色联盟信息表';

-- --------------------------------------------------------

--
-- 添加表，vip表
-- daer
-- 2017081503
--

DROP TABLE IF EXISTS `db_tab_role_vip`;
CREATE TABLE IF NOT EXISTS `db_tab_role_vip`
(
    `role_id`   bigint(20) NOT NULL COMMENT '角色id',
    `vip_level` int(11)    NOT NULL COMMENT 'vip等级',
    `exp`       int(11)    NOT NULL COMMENT 'vip等级',
    `gift_list` mediumblob COMMENT '普通奖励',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='vip表';

-- --------------------------------------------------------

--
-- 添加表，城池拜访表
-- hyq
-- 2020091801
--

DROP TABLE IF EXISTS `db_tab_role_visit_friend`;
CREATE TABLE IF NOT EXISTS `db_tab_role_visit_friend`
(
    `role_id`       bigint(20) NOT NULL COMMENT '角色id',
    `visit_times`   int(11)    NOT NULL COMMENT '进行拜访, 被不喜欢次数',
    `visited_times` int(11)    NOT NULL COMMENT '被拜访次数',
    `visit_state`   int(11)    NOT NULL COMMENT '拜访状态',
    `sad_time`      int(11)    NOT NULL COMMENT '心碎开始时间',
    `visited_list`  mediumblob COMMENT '来访记录',
    `visit_list`    mediumblob COMMENT '拜访记录',
    `receive_list`  mediumblob COMMENT '收到的礼品列表',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='活动-城池拜访';

-- --------------------------------------------------------

--
-- 添加表，斥候数据
-- daer
-- 2017080204
--

DROP TABLE IF EXISTS `db_tab_role_warrior`;
CREATE TABLE IF NOT EXISTS `db_tab_role_warrior`
(
    `role_id`           bigint(20) NOT NULL COMMENT '角色id',
    `warrior_list`      mediumblob COMMENT '斥候列表',
    `explore_list`      mediumblob COMMENT '探索列表',
    `search_list`       mediumblob COMMENT '查探列表',
    `train_list`        mediumblob COMMENT '训练列表',
    `result_list`       mediumblob COMMENT '探索结果列表',
    `last_explore_time` int(11)    NOT NULL COMMENT '上次探索的时间',
    `train_end_time`    bigint(20) NOT NULL COMMENT '训练结束时间',
    `last_remain_time`  int(11)    NOT NULL COMMENT '最近一次停止训练时剩余时间',
    `occupy_list`       mediumblob COMMENT '占领过的地块进化物列表',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='斥候数据';

-- --------------------------------------------------------

--
-- 新增表，权重礼包
-- dengjc
-- 2021021817
--

DROP TABLE IF EXISTS `db_tab_role_weight_gift`;
CREATE TABLE IF NOT EXISTS `db_tab_role_weight_gift`
(
    `role_id`     bigint(20) UNSIGNED NOT NULL DEFAULT '0' COMMENT '玩家id',
    `weight_list` mediumblob COMMENT '权重列表',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='权重礼包';

-- --------------------------------------------------------

--
-- 添加表，个人世界掉落表
-- hyq
-- 2020062011
--

DROP TABLE IF EXISTS `db_tab_role_world_drop`;
CREATE TABLE IF NOT EXISTS `db_tab_role_world_drop`
(
    `role_id`        bigint(20) NOT NULL COMMENT '角色id',
    `drop_time_list` mediumblob COMMENT '掉落时间列表',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='个人世界掉落表';

-- --------------------------------------------------------

--
-- 添加表，sdk禁言表
-- djc
-- 2020070715
--

DROP TABLE IF EXISTS `db_tab_sdk_ban`;
CREATE TABLE IF NOT EXISTS `db_tab_sdk_ban`
(
    `role_id`  bigint(20) NOT NULL COMMENT '角色id',
    `ban_time` int(11) DEFAULT '0' COMMENT '禁言时间',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='sdk禁言';

-- --------------------------------------------------------

--
-- 添加表，商店信息
-- daer
-- 2017080207
--

DROP TABLE IF EXISTS `db_tab_shop_info`;
CREATE TABLE IF NOT EXISTS `db_tab_shop_info`
(
    `role_id`          bigint(20) NOT NULL COMMENT '角色id',
    `has_buy_list`     mediumblob COMMENT '已购买公共商店物品列表',
    `unusal_last_time` int(11)    NOT NULL COMMENT '公共商店上次刷新时间',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='商店信息';

-- --------------------------------------------------------

--
-- 添加表，【精选招募】- 全服事件记录
-- hyq
-- 2021020301
--

DROP TABLE IF EXISTS `db_tab_spec_event`;
CREATE TABLE IF NOT EXISTS `db_tab_spec_event`
(
    `id`                int(11)     NOT NULL AUTO_INCREMENT,
    `role_id`           bigint(20)  NOT NULL COMMENT '角色id',
    `name`              varchar(20) NOT NULL COMMENT '角色名',
    `head_id`           int(11)     NOT NULL COMMENT '头像id',
    `frame_id`          int(11)     NOT NULL COMMENT '头像框id',
    `event_type`        int(11)     NOT NULL COMMENT '事件类型',
    `activity_type`     int(11)     NOT NULL COMMENT '活动类型',
    `recruit_type`      int(11)     NOT NULL COMMENT '招募类型',
    `general_type_list` tinyblob COMMENT '武将类型列表',
    `attach_time`       int(11)     NOT NULL COMMENT '事件触发时间',
    `attach_way`        int(11)     NOT NULL COMMENT '触发方式',
    PRIMARY KEY (`id`),
    KEY `activity_type` (`activity_type`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='精选招募-全服事件记录';

-- --------------------------------------------------------

--
-- 新增表，召唤怪关联表
-- xiewendong
-- 2020122811
-- 

DROP TABLE IF EXISTS `db_tab_summon_mon`;
CREATE TABLE IF NOT EXISTS `db_tab_summon_mon`
(
    `army_id`          bigint(20) UNSIGNED NOT NULL DEFAULT '0' COMMENT '军队id',
    `summon_army_list` mediumblob COMMENT '召唤怪数据',
    PRIMARY KEY (`army_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='召唤怪关联表';

-- --------------------------------------------------------

--
-- 添加表，系统数据表
-- zsk
-- 2017091101
--

DROP TABLE IF EXISTS `db_tab_system_data`;
CREATE TABLE IF NOT EXISTS `db_tab_system_data`
(
    `key` int(11)    NOT NULL COMMENT 'key',
    `val` mediumblob NOT NULL COMMENT 'val',
    PRIMARY KEY (`key`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='系统数据表';

-- --------------------------------------------------------

--
-- 添加表，玩家任务
-- daer
-- 2017081504
--

DROP TABLE IF EXISTS `db_tab_task_misc`;
CREATE TABLE IF NOT EXISTS `db_tab_task_misc`
(
    `role_id`            bigint(20) NOT NULL COMMENT '角色id',
    `new_player_show`    mediumblob COMMENT 'vip等级',
    `daily_point`        int(11)    NOT NULL COMMENT 'vip等级',
    `daily_box`          mediumblob COMMENT '普通奖励',
    `chapter`            mediumblob COMMENT '普通奖励',
    `strategy_task_data` mediumblob COMMENT '战略目标任务信息',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='玩家任务';

-- --------------------------------------------------------

--
-- 添加表，联盟申请表
-- lin
-- 2017082601
--

DROP TABLE IF EXISTS `db_tab_union_apply`;
CREATE TABLE IF NOT EXISTS `db_tab_union_apply`
(
    `apply_id`   bigint(20) NOT NULL COMMENT '申请id',
    `role_id`    bigint(20) NOT NULL COMMENT '申请角色id',
    `union_id`   bigint(20) NOT NULL COMMENT '联盟id',
    `apply_time` int(11)    NOT NULL COMMENT '申请时间戳',
    PRIMARY KEY (`apply_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='联盟成员表';

-- --------------------------------------------------------

--
-- 添加表，联盟基础表
-- lin
-- 2017082301
--

DROP TABLE IF EXISTS `db_tab_union_base`;
CREATE TABLE IF NOT EXISTS `db_tab_union_base`
(
    `union_id`              bigint(20)       NOT NULL COMMENT '联盟id',
    `union_head`            int(11)          NOT NULL COMMENT '联盟头像',
    `union_credit`          int(11)          NOT NULL COMMENT '联盟当前声望（其实就是联盟经验，用于联盟升级）',
    `union_level`           int(11)          NOT NULL COMMENT '联盟等级',
    `union_name`            varchar(20)      NOT NULL COMMENT '联盟名称',
    `union_leader_id`       bigint(20)       NOT NULL COMMENT '联盟主角色id',
    `union_desc`            mediumblob COMMENT '联盟宣言',
    `union_rct_status`      int(11)          NOT NULL COMMENT '联盟招募状态，1公开招募；2审核招募',
    `role_id_list`          mediumblob COMMENT '联盟成员id列表',
    `apply_id_list`         mediumblob COMMENT '联盟申请id列表',
    `log_id_list`           mediumblob COMMENT '联盟事件id列表',
    `union_impeach`         mediumblob COMMENT '联盟弹劾信息',
    `help_list`             longblob COMMENT '联盟帮助列表',
    `sign_list`             mediumblob COMMENT '联盟标记列表',
    `had_rank_first`        int(11)          NOT NULL COMMENT '是否有过排行榜第一',
    `had_rank_three`        int(11)          NOT NULL COMMENT '是否有过排行榜前三',
    `had_rank_ten`          int(11)          NOT NULL COMMENT '是否有个排行榜前十',
    `task_point`            int(11)          NOT NULL COMMENT '联盟任务点数（成员每日完成了多少个联盟任务）',
    `day_union_lev`         int(11)          NOT NULL COMMENT '联盟每日等级（0点记录,联盟任务宝箱需要）',
    `task_box`              mediumblob COMMENT '联盟任务宝箱列表',
    `last_refresh_box_time` int(11)          NOT NULL COMMENT '上次刷新联盟任务宝箱时间戳',
    `team_list`             mediumblob COMMENT '联盟小队列表',
    `battle_city_list`      mediumblob COMMENT '已经宣战的城池id列表',
    `auto_transfer_time`    int(11)          NOT NULL COMMENT '自动转让盟主到点时间戳',
    `release_data`          mediumblob COMMENT '神秘卡发布信息',
    `use_list`              mediumblob COMMENT '世族印玺使用记录，凌晨清空',
    `announce_use_list`     mediumblob COMMENT '招募喊话次数',
    `task_data`             mediumblob COMMENT '联盟任务数据',
    `seal_rank_list`        mediumblob COMMENT '印玺排行',
    `seal_recently_list`    mediumblob COMMENT '最近几天的盖印信息',
    `set_proxy_leader_time` int(11)          NOT NULL DEFAULT '0' COMMENT '自动设置代理盟主的时间',
    `voice_info`            mediumblob       NOT NULL COMMENT '语言聊天信息',
    `create_time`           int(10) UNSIGNED NOT NULL DEFAULT '0' COMMENT '创建时间',
    `buy_times_list`        mediumblob COMMENT '世族官职对应的印玺购买次数列表',
    `leader_change_time`    int(10) UNSIGNED NOT NULL DEFAULT '0' COMMENT '宗主职位变更目标时间戳',
    `official_time_list`    mediumblob COMMENT '世族官职对应的变更目标时间戳列表，[{official, time} |_]',
    PRIMARY KEY (`union_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='联盟基础表';

-- --------------------------------------------------------

--
-- 添加表，联盟外交表 @todo (这种结构待改，每个联盟都存有一份数据，太麻烦，关系变了都要调双方的进程更新)
-- lei
-- 2017090601
--

DROP TABLE IF EXISTS `db_tab_union_diplomacy`;
CREATE TABLE IF NOT EXISTS `db_tab_union_diplomacy`
(
    `union_id`             bigint(20) NOT NULL COMMENT '联盟id',
    `diplomacy_list`       mediumblob COMMENT '外交列表',
    `diplomacy_event_list` mediumblob COMMENT '外交事件列表',
    `belong_id`            bigint(20) NOT NULL COMMENT '属主id',
    `sub_id_list`          mediumblob COMMENT '附属世族id列表',
    `dip_color`            int(11)    NOT NULL COMMENT '外交颜色',
    `can_unite_time`       int(11)             DEFAULT '0' COMMENT '取消同盟的时间',
    `del_diplomacy_list`   mediumblob NOT NULL COMMENT '主动操作定时器信息',
    `can_sub_time`         int(11)    NOT NULL DEFAULT '0' COMMENT '可附属时间',
    PRIMARY KEY (`union_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='联盟外交表';

-- --------------------------------------------------------

--
-- 添加表，联盟外交好友度表
-- lei
-- 2017090701
--

DROP TABLE IF EXISTS `db_tab_union_dip_degree`;
CREATE TABLE IF NOT EXISTS `db_tab_union_dip_degree`
(
    `id_couple`      varchar(255) NOT NULL COMMENT '联盟id对',
    `degree`         int(11) DEFAULT NULL COMMENT '好友度',
    `last_save_time` int(11) DEFAULT NULL COMMENT '上次保存好友度时间',
    `status`         int(11) DEFAULT NULL COMMENT '外交状态',
    PRIMARY KEY (`id_couple`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='联盟外交好友度表';

-- --------------------------------------------------------

--
-- 添加表，联盟捐献表
-- lin
-- 2017082901
--

DROP TABLE IF EXISTS `db_tab_union_donate`;
CREATE TABLE IF NOT EXISTS `db_tab_union_donate`
(
    `union_id`               bigint(20) NOT NULL COMMENT '联盟id',
    `donate_credit`          int(11)    NOT NULL COMMENT '今日累计声望值',
    `old_level`              int(11)    NOT NULL COMMENT '上次清空声望值时的联盟等级',
    `last_empty_credit_time` int(11)    NOT NULL COMMENT '上次清空今日累计声望值时间戳',
    PRIMARY KEY (`union_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='联盟捐献表';

-- --------------------------------------------------------

--
-- 添加表，联盟国家世族信息表（world_union_srv管理）
-- lin
-- 2017082401
--

DROP TABLE IF EXISTS `db_tab_union_family`;
CREATE TABLE IF NOT EXISTS `db_tab_union_family`
(
    `union_id`             bigint(20) NOT NULL COMMENT '联盟id',
    `union_type`           int(11)    NOT NULL COMMENT '联盟世族类型。1宗室，2公室，3王室',
    `country_id`           int(11)    NOT NULL COMMENT '联盟所属国家id',
    `admin_country_list`   mediumblob COMMENT '拥有的国家国都城池id列表（包括周、中山、卫，宋）',
    `leader_official`      int(11)    NOT NULL COMMENT '盟主官爵',
    `city_id_list`         mediumblob COMMENT '联盟城池id列表',
    `history_city_id_list` mediumblob COMMENT '占领过的联盟城池id列表',
    `city_office_list`     mediumblob COMMENT '郡守、县令玩家ID列表',
    `event_list`           mediumblob COMMENT '国家事件列表。[#r_union_country_event{}, ...]',
    `union_sink`           mediumblob COMMENT '世族沦陷信息',
    `force`                bigint(20) NOT NULL COMMENT '世族势力值',
    `center_city_id`       bigint(20) NOT NULL DEFAULT '0' COMMENT '国都的城池id',
    `nation_title`         int(11)    NOT NULL DEFAULT '0' COMMENT '联盟国号',
    `nation_banner`        int(11)    NOT NULL DEFAULT '0' COMMENT '联盟旗帜',
    `top_history`          blob       NOT NULL COMMENT '联盟国家历史达到最强的信息',
    `move_city_info`       blob       NOT NULL COMMENT '迁都信息',
    `nation_red_dot`       mediumblob NOT NULL COMMENT '国家红点信息',
    `self_pass_list`       mediumblob COMMENT '联盟自建关卡id列表',
    PRIMARY KEY (`union_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='联盟国家世族信息表';

-- --------------------------------------------------------

--
-- 添加表，联盟内部排行榜
-- Karl
-- 2020071302
--

DROP TABLE IF EXISTS `db_tab_union_inner_rank`;
CREATE TABLE IF NOT EXISTS `db_tab_union_inner_rank`
(
    `union_id`    bigint(20) NOT NULL DEFAULT '0' COMMENT '联盟id',
    `donate_rank` mediumblob COMMENT '捐献排行',
    `smith_rank`  mediumblob COMMENT '工匠排行',
    PRIMARY KEY (`union_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='联盟内部排行榜';

-- --------------------------------------------------------

--
-- 添加表，联盟事件表
-- lin
-- 2017082701
--

DROP TABLE IF EXISTS `db_tab_union_log`;
CREATE TABLE IF NOT EXISTS `db_tab_union_log`
(
    `log_id`    bigint(20) NOT NULL COMMENT '事件id',
    `union_id`  bigint(20) NOT NULL COMMENT '联盟id',
    `log_tid`   int(11)    NOT NULL COMMENT '配置id',
    `log_time`  int(11)    NOT NULL COMMENT '申请时间戳',
    `args_list` mediumblob NOT NULL COMMENT '参数列表',
    PRIMARY KEY (`log_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='联盟事件表';

-- --------------------------------------------------------

--
-- 添加表，联盟成员表
-- lin
-- 2017082501
--

DROP TABLE IF EXISTS `db_tab_union_member`;
CREATE TABLE IF NOT EXISTS `db_tab_union_member`
(
    `role_id`              bigint(20)       NOT NULL COMMENT '成员角色id',
    `union_id`             bigint(20)       NOT NULL COMMENT '联盟id',
    `job`                  int(11)          NOT NULL COMMENT '成员职位',
    `join_time`            int(11)          NOT NULL COMMENT '加入联盟时间戳',
    `is_join`              tinyint(4)       NOT NULL COMMENT '是否在联盟中',
    `last_offline_time`    int(11)          NOT NULL COMMENT '上次离线时间戳',
    `out_union_time`       int(11)          NOT NULL COMMENT '上次退出联盟时间戳',
    `donate_num`           int(11)          NOT NULL COMMENT '今日已使用的捐献次数',
    `last_rfs_donate_time` int(11)          NOT NULL COMMENT '上次刷新捐献次数时间戳',
    `wish_num`             int(11)          NOT NULL COMMENT '今日已使用的许愿次数',
    `last_rfs_wish_time`   int(11)          NOT NULL COMMENT '上次刷新许愿次数时间戳',
    `donate`               int(11)          NOT NULL COMMENT '历史贡献值（加入该联盟之后对该联盟的贡献值，退出后清空）',
    `official`             int(11)          NOT NULL COMMENT '联盟官爵、上次退出联盟时的联盟官爵',
    `exploits`             int(11)          NOT NULL COMMENT '累计战功',
    `job_index`            int(11)          NOT NULL COMMENT '招募喊话',
    `is_proxy_leader`      tinyint(4)       NOT NULL DEFAULT '0' COMMENT '是否代理宗主1是',
    `donate_cd`            int(10) UNSIGNED NOT NULL DEFAULT '0' COMMENT '世族捐献CD时间戳',
    PRIMARY KEY (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='联盟成员表';

-- --------------------------------------------------------

--
-- 添加表，联盟资源表
-- lin
-- 2017082801
--

DROP TABLE IF EXISTS `db_tab_union_res`;
CREATE TABLE IF NOT EXISTS `db_tab_union_res`
(
    `union_id`    bigint(20) NOT NULL COMMENT '联盟id',
    `union_wood`  bigint(20) NOT NULL COMMENT '联盟木材',
    `union_food`  bigint(20) NOT NULL COMMENT '联盟粮食',
    `union_iron`  bigint(20) NOT NULL COMMENT '联盟铁矿',
    `union_stone` bigint(20) NOT NULL COMMENT '联盟石材',
    PRIMARY KEY (`union_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='联盟资源表';

-- --------------------------------------------------------

--
-- 添加表，联盟定时科技
-- daer
-- 2021070708
--

DROP TABLE IF EXISTS `db_tab_union_time_effect`;
CREATE TABLE IF NOT EXISTS `db_tab_union_time_effect`
(
    `union_id`    bigint(20) NOT NULL COMMENT '联盟id',
    `effect_list` mediumblob COMMENT '列表',
    PRIMARY KEY (`union_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='联盟定时科技';

-- --------------------------------------------------------

--
-- 添加表，联盟许愿表
-- lin
-- 2017083001
--

DROP TABLE IF EXISTS `db_tab_union_wish`;
CREATE TABLE IF NOT EXISTS `db_tab_union_wish`
(
    `union_id`             bigint(20) NOT NULL COMMENT '联盟id',
    `wish_list`            mediumblob COMMENT '心愿列表 [#wish{}]',
    `last_empty_wish_time` int(11)    NOT NULL COMMENT '上次清空心愿列表时间戳',
    PRIMARY KEY (`union_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='联盟许愿表';

-- --------------------------------------------------------

--
-- 添加表，世界活动表
-- zsk
-- 2018111301
--

DROP TABLE IF EXISTS `db_tab_world_activity`;
CREATE TABLE IF NOT EXISTS `db_tab_world_activity`
(
    `activity_id` int(11)    NOT NULL COMMENT '活动Id',
    `open_state`  int(11)    NOT NULL COMMENT '开启状态',
    `start_time`  int(11)    NOT NULL COMMENT '开始时间',
    `close_time`  int(11)    NOT NULL COMMENT '关闭时间',
    `time_index`  bigint(20) NOT NULL COMMENT '活动的开启时间和结束时间的时间戳相加',
    `unique_id`   bigint(20) NOT NULL DEFAULT '0' COMMENT '实例id',
    PRIMARY KEY (`activity_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='世界活动表';

-- --------------------------------------------------------

--
-- 添加表，每日卡牌
-- djc
-- 2020042209
--

DROP TABLE IF EXISTS `db_tab_world_daily_card`;
CREATE TABLE IF NOT EXISTS `db_tab_world_daily_card`
(
    `id`          int(11)    NOT NULL COMMENT 'id',
    `today_cards` mediumblob NOT NULL COMMENT '今日卡牌',
    `refreshed`   mediumblob NOT NULL COMMENT '已刷新的卡牌',
    PRIMARY KEY (`id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='每日卡牌';

-- --------------------------------------------------------

--
-- 添加表，年兽赠礼怪物信息
-- gjx
-- 2021020101
--

DROP TABLE IF EXISTS `db_tab_world_nian`;
CREATE TABLE IF NOT EXISTS `db_tab_world_nian`
(
    `activity_id`    int(11)     NOT NULL COMMENT '活动ID',
    `open`           smallint(6) NOT NULL COMMENT '是否开始统计击杀怪物',
    `kill_num`       int(11)     NOT NULL COMMENT '全服已杀死的雪人数量',
    `born_num`       int(11)     NOT NULL COMMENT '已生成年兽数量',
    `random_country` blob COMMENT '本轮已随机过的省份ID',
    `rounds`         int(11)     NOT NULL COMMENT '轮次',
    `random_data`    mediumblob COMMENT '本轮已随机过的省份和坐标',
    PRIMARY KEY (`activity_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='年兽赠礼活动信息';

-- --------------------------------------------------------

--
-- 添加表，年兽怪物信息
-- daer
-- 2021070709
--

DROP TABLE IF EXISTS `db_tab_world_nian_mon`;
CREATE TABLE IF NOT EXISTS `db_tab_world_nian_mon`
(
    `nian_id`       int(11)     NOT NULL COMMENT '年兽id',
    `conf_id`       smallint(6) NOT NULL COMMENT '年兽配置id',
    `activity_id`   int(11)     NOT NULL COMMENT '对应的活动ID',
    `x`             int(11)     NOT NULL COMMENT 'x',
    `y`             int(11)     NOT NULL COMMENT 'y',
    `country_id`    int(11)     NOT NULL COMMENT '国家ID',
    `join_role_ids` blob COMMENT '加入战斗的玩家id',
    `army_id_list`  blob COMMENT '所有军队ID',
    `del_time`      int(11)     NOT NULL COMMENT '删除怪物时间戳',
    PRIMARY KEY (`nian_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='年兽赠礼怪物信息';

-- --------------------------------------------------------

--
-- 添加表，雪人活动的雪人信息
-- djc
-- 2020122802
--

DROP TABLE IF EXISTS `db_tab_world_snowman`;
CREATE TABLE IF NOT EXISTS `db_tab_world_snowman`
(
    `id`            bigint(20)  NOT NULL COMMENT '雪人对应的怪物id',
    `conf_id`       smallint(6) NOT NULL COMMENT '雪人配置id',
    `x`             int(11)     NOT NULL COMMENT 'x',
    `y`             int(11)     NOT NULL COMMENT 'y',
    `creator`       bigint(20)  NOT NULL COMMENT '创建人',
    `materials`     blob COMMENT '材料',
    `state`         smallint(6) NOT NULL COMMENT '0收集中, 1收集完毕',
    `end_time`      int(11)     NOT NULL COMMENT '结束时间',
    `is_open`       smallint(6) NOT NULL COMMENT '是否开放给世界的人加入，0否1是',
    `join_role_ids` blob COMMENT '加入战斗的玩家id',
    `max_hp`        int(11)     NOT NULL COMMENT '最大血量',
    `att_hp`        int(11)     NOT NULL COMMENT '已攻打血量',
    PRIMARY KEY (`id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='雪人活动的雪人信息';

-- --------------------------------------------------------

--
-- 添加表，世界任务表
-- zsk
-- 2017081901
--

DROP TABLE IF EXISTS `db_tab_world_task`;
CREATE TABLE IF NOT EXISTS `db_tab_world_task`
(
    `task_id`      bigint(20) NOT NULL COMMENT '任务id',
    `target_type`  bigint(20) NOT NULL COMMENT '目标类型（全服，国家，指定联盟）',
    `target`       bigint(20)          DEFAULT NULL COMMENT '目标对象（国家，联盟）',
    `config_id`    int(11)    NOT NULL COMMENT '配置表ID',
    `start_time`   int(11)    NOT NULL COMMENT '开始时间',
    `finish_time`  int(11)             DEFAULT NULL COMMENT '任务完成时间',
    `open_state`   int(11)    NOT NULL COMMENT '任务开启状态',
    `state`        int(11)    NOT NULL COMMENT '任务状态',
    `count`        int(11)    NOT NULL COMMENT '计数器',
    `finish_info`  mediumblob COMMENT '扩展字段',
    `extend`       mediumblob COMMENT '扩展字段',
    `recruit_info` mediumblob COMMENT '招募信息',
    `end_time`     int(11)    NOT NULL DEFAULT '0' COMMENT '任务结束时间',
    PRIMARY KEY (`task_id`),
    KEY `task_id` (`task_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4;

-- --------------------------------------------------------

--
-- 添加表，天子宝库活动表
-- daer
-- 2021070710
--

DROP TABLE IF EXISTS `db_tab_world_treasury`;
CREATE TABLE IF NOT EXISTS `db_tab_world_treasury`
(
    `activity_id`      int(11) UNSIGNED     NOT NULL COMMENT '活动ID',
    `open`             smallint(5) UNSIGNED NOT NULL COMMENT '是否开始活动',
    `random_data`      mediumblob           NOT NULL COMMENT '随机信息[{CountryId,[{X,Y}...]}]',
    `need_revive_list` mediumblob           NOT NULL COMMENT '需要复活的列表',
    PRIMARY KEY (`activity_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='天子宝库活动表';

-- --------------------------------------------------------

--
-- 添加表，天子宝库城池表
-- daer
-- 2021070711
-- 

DROP TABLE IF EXISTS `db_tab_world_treasury_city`;
CREATE TABLE IF NOT EXISTS `db_tab_world_treasury_city`
(
    `city_id`        bigint(20) UNSIGNED NOT NULL COMMENT '城池id',
    `activity_id`    int(11) UNSIGNED    NOT NULL COMMENT '活动id',
    `create_time`    int(11) UNSIGNED    NOT NULL COMMENT '创建时间',
    `resource_times` int(11) UNSIGNED    NOT NULL COMMENT '已产生资源的次数',
    `resource_type`  int(11) UNSIGNED    NOT NULL COMMENT '生产的资源类型',
    `resource_num`   int(11) UNSIGNED    NOT NULL COMMENT '当前生产的资源数量',
    `occupy_time`    int(11) UNSIGNED    NOT NULL COMMENT '占领时间',
    `rob_num`        int(11) UNSIGNED    NOT NULL COMMENT '被掠夺数量',
    `rob_time`       int(11) UNSIGNED    NOT NULL COMMENT '最后一次被掠夺的时间',
    `rob_log`        mediumblob          NOT NULL COMMENT '掠夺日志',
    `template_id`    varchar(255)        NOT NULL COMMENT '模板id',
    PRIMARY KEY (`city_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='天子宝库城池表';


-- --------------------------------------------------------

--
-- 添加表，野外废墟
-- huanghaifeng
-- 2021071501
--

DROP TABLE IF EXISTS `db_tab_city_group_scrap`;
CREATE TABLE IF NOT EXISTS `db_tab_city_group_scrap`
(
    `city_group_id`   int(11) UNSIGNED    NOT NULL COMMENT '城池组唯一id',
    `tid`             int(11) UNSIGNED    NOT NULL COMMENT '配置表tid',
    `host_id`         bigint(20) UNSIGNED NOT NULL COMMENT '归属者id',
    `last_host_id`    bigint(20) UNSIGNED NOT NULL COMMENT '上一个归属者id(野外废墟才有)',
    `x`               int(11) UNSIGNED    NOT NULL COMMENT 'x坐标',
    `y`               int(11) UNSIGNED    NOT NULL COMMENT 'y坐标',
    `rebuild_city_id` bigint(20) UNSIGNED NOT NULL COMMENT '改建后的城池id,为0表示未改建',
    `fun_ext`         mediumblob          NOT NULL COMMENT '拓展功能字段 (不同城池组有不同结构，具体看代码)',
    `extend`          mediumblob          NOT NULL COMMENT '杂项信息',
    PRIMARY KEY (`city_group_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='废墟城池组表';

-- --------------------------------------------------------

--
-- 添加表，数据库版本
-- daer
-- 2021070712
--

DROP TABLE IF EXISTS `db_version`;
CREATE TABLE IF NOT EXISTS `db_version`
(
    `version` varchar(32) NOT NULL COMMENT '版本',
    PRIMARY KEY (`version`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='当前数据库版本，更新数据库时使用';


--
-- 添加表，怪物队伍信息
-- daer
-- 2021111101
--

DROP TABLE IF EXISTS `db_tab_team_data`;
CREATE TABLE IF NOT EXISTS `db_tab_team_data`
(
    `team_id`        bigint(20) UNSIGNED NOT NULL COMMENT '小队id',
    `total_wave`     int(11) UNSIGNED    NOT NULL COMMENT '总波数',
    `leader_id`      bigint(20) UNSIGNED NOT NULL COMMENT '领队id',
    `total_army_num` int(11) UNSIGNED    NOT NULL COMMENT '总的军队数量',
    `wave_army_list` mediumblob          NOT NULL COMMENT '每波的军队id',
    PRIMARY KEY (`team_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='波次怪小队信息';



COMMIT;