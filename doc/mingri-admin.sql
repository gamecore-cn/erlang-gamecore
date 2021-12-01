--
-- 管理后台数据库 --
-- 格式如下

--
-- 添加表，注释/添加xx字段
-- author
-- YYYYMMDDHH（如：2017080901，格式和长度都不能更改）
--
--
-- 重要说明：
-- 1. 修改表字段时，内容必须在原表及所有对该表的修改语句之后，最好是加到文件尾
-- 2. 时间要使用修改当日的时间，不能随意写（升级脚本自动生成过程和版本管理均依赖于时间，写错将导致数据库无法正常维护！！！）
-- 3. 时间不可重复，如果当前小时已被占用，使用下一小时
-- 4. 较新时间的语句可以放在较旧时间的语句之前，但不推荐这么做
--

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
START TRANSACTION;
SET time_zone = "+00:00";

-- --------------------------------------------------------

--
-- 添加表，设置数据库版本
-- daer
-- 2017080101
--

DROP TABLE IF EXISTS `db_version`;
CREATE TABLE IF NOT EXISTS `db_version`
(
    `version` varchar(32) NOT NULL COMMENT '版本',
    PRIMARY KEY (`version`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='当前数据库版本，更新数据库时使用';

-- --------------------------------------------------------

--
-- 添加表，活跃度统计表
-- zsk
-- 2019010702
--

DROP TABLE IF EXISTS `log_tab_activity_point`;
CREATE TABLE IF NOT EXISTS `log_tab_activity_point`
(
    `id`             int(11)     NOT NULL AUTO_INCREMENT,
    `agent_name`     varchar(20) NOT NULL COMMENT '代理名',
    `role_id`        bigint(20)  NOT NULL COMMENT '角色id',
    `activity_point` int(11)     NOT NULL COMMENT '活跃度',
    `m_time`         int(11)     NOT NULL COMMENT '时间戳',
    PRIMARY KEY (`id`),
    KEY `m_time` (`m_time`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='活跃度统计表';

-- --------------------------------------------------------

--
-- 添加表，出征信息统计
-- zsk
-- 2019010703
--

DROP TABLE IF EXISTS `log_tab_army_fight`;
CREATE TABLE IF NOT EXISTS `log_tab_army_fight`
(
    `id`         int(11)     NOT NULL AUTO_INCREMENT,
    `agent_name` varchar(20) NOT NULL COMMENT '代理名',
    `acc_name`   varchar(64) NOT NULL COMMENT '账号名',
    `role_id`    bigint(20)  NOT NULL COMMENT '角色id',
    `role_name`  varchar(20) NOT NULL COMMENT '玩家名',
    `type`       int(11)     NOT NULL COMMENT '出征类型（普通出征为0，攻城战为1）',
    `m_time`     int(11)     NOT NULL COMMENT '时间戳',
    PRIMARY KEY (`id`),
    KEY `m_time` (`m_time`),
    KEY `role_id` (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='出征信息统计';

-- --------------------------------------------------------

--
-- 添加表，服务器城池占领信息
-- zsk
-- 2019010704
--

DROP TABLE IF EXISTS `log_tab_city_occupy`;
CREATE TABLE IF NOT EXISTS `log_tab_city_occupy`
(
    `id`         int(11)     NOT NULL AUTO_INCREMENT,
    `agent_name` varchar(20) NOT NULL COMMENT '代理名',
    `xiancheng`  int(11)     NOT NULL COMMENT '县城占领数量',
    `juncheng`   int(11)     NOT NULL COMMENT '郡城占领数量',
    `zhoucheng`  int(11)     NOT NULL COMMENT '州城占领数量',
    `wangcheng`  int(11)     NOT NULL COMMENT '王城占领数量',
    `m_time`     int(11)     NOT NULL COMMENT '时间戳',
    PRIMARY KEY (`id`),
    KEY `m_time` (`m_time`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='服务器城池占领信息';

-- --------------------------------------------------------

--
-- 添加表，采集物能量投放表
-- Tom
-- 2021011601
--

DROP TABLE IF EXISTS `log_tab_coll_power`;
CREATE TABLE IF NOT EXISTS `log_tab_coll_power`
(
    `id`            int(11) UNSIGNED NOT NULL AUTO_INCREMENT,
    `pvp_power`     int(11) UNSIGNED NOT NULL COMMENT '玩家部队pvp充能',
    `city_power`    int(11) UNSIGNED NOT NULL COMMENT '分城主城充能',
    `spec_power`    int(11) UNSIGNED NOT NULL COMMENT '特殊充能',
    `release_power` int(11) UNSIGNED NOT NULL COMMENT '投放总能量',
    `m_time`        int(11) UNSIGNED NOT NULL COMMENT '投放能量时间戳',
    PRIMARY KEY (`id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='采集物能量投放表';

-- --------------------------------------------------------

--
-- 添加表，元宝购买道具消耗表
-- zsk
-- 2019010705
--

DROP TABLE IF EXISTS `log_tab_diamond_buy`;
CREATE TABLE IF NOT EXISTS `log_tab_diamond_buy`
(
    `id`         int(11)      NOT NULL AUTO_INCREMENT,
    `agent_name` varchar(20)  NOT NULL COMMENT '代理名',
    `channel`    varchar(20)  NOT NULL COMMENT '渠道名',
    `role_id`    bigint(20)   NOT NULL COMMENT '角色id',
    `cost`       int(11)      NOT NULL COMMENT '消耗元宝数量',
    `goods_type` mediumint(9) NOT NULL COMMENT '购买物品类型（资源类型）',
    `goods_num`  int(11)      NOT NULL COMMENT '购买道具数量',
    `goods_name` varchar(20)  NOT NULL COMMENT '购买道具名',
    `m_time`     int(11)      NOT NULL COMMENT '时间戳',
    PRIMARY KEY (`id`),
    KEY `m_time` (`m_time`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='元宝购买道具消耗表';

-- --------------------------------------------------------

--
-- 添加表，服务器资源团占领信息
-- zsk
-- 2019010706
--

DROP TABLE IF EXISTS `log_tab_grid_occupy`;
CREATE TABLE IF NOT EXISTS `log_tab_grid_occupy`
(
    `id`         int(11)     NOT NULL AUTO_INCREMENT,
    `agent_name` varchar(20) NOT NULL COMMENT '代理名',
    `grid_num_1` int(11)     NOT NULL COMMENT '地块等级1占领数量',
    `grid_num_2` int(11)     NOT NULL COMMENT '地块等级2占领数量',
    `grid_num_3` int(11)     NOT NULL COMMENT '地块等级3占领数量',
    `grid_num_4` int(11)     NOT NULL COMMENT '地块等级4占领数量',
    `grid_num_5` int(11)     NOT NULL COMMENT '地块等级5占领数量',
    `grid_num_6` int(11)     NOT NULL COMMENT '地块等级6占领数量',
    `grid_num_7` int(11)     NOT NULL COMMENT '地块等级7占领数量',
    `grid_num_8` int(11)     NOT NULL COMMENT '地块等级8占领数量',
    `grid_num_9` int(11)     NOT NULL COMMENT '地块等级9占领数量',
    `m_time`     int(11)     NOT NULL COMMENT '时间戳',
    PRIMARY KEY (`id`),
    KEY `m_time` (`m_time`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='服务器资源团占领信息';

-- --------------------------------------------------------

--
-- 新增表，`log_tab_optional_recruit`
-- daer
-- 2021070701
--

DROP TABLE IF EXISTS `log_tab_optional_recruit`;
CREATE TABLE IF NOT EXISTS `log_tab_optional_recruit`
(
    `id`         int(11) UNSIGNED    NOT NULL AUTO_INCREMENT,
    `role_id`    bigint(20) UNSIGNED NOT NULL COMMENT '角色id',
    `acc_name`   varchar(64)         NOT NULL COMMENT '账号名',
    `role_name`  varchar(20)         NOT NULL COMMENT '玩家名',
    `recruit_id` bigint(20)          NOT NULL DEFAULT '0' COMMENT '卡池唯一ID',
    `left_time`  int(11) UNSIGNED    NOT NULL COMMENT '卡池的可抽次数',
    `pre_times`  int(11) UNSIGNED    NOT NULL COMMENT '触发之前抽了多少次',
    `m_time`     int(11) UNSIGNED    NOT NULL COMMENT '时间戳',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4;

-- --------------------------------------------------------

--
-- 添加表，玩法记录表
-- zsk
-- 2019010707
--

DROP TABLE IF EXISTS `log_tab_play_way`;
CREATE TABLE IF NOT EXISTS `log_tab_play_way`
(
    `id`         int(11)     NOT NULL AUTO_INCREMENT,
    `agent_name` varchar(20) NOT NULL COMMENT '代理名',
    `channel`    varchar(20) NOT NULL COMMENT '渠道名',
    `role_id`    bigint(20)  NOT NULL COMMENT '角色id',
    `play_type`  int(11)     NOT NULL COMMENT '玩法类型',
    `num`        int(11)     NOT NULL COMMENT '次数',
    `m_time`     int(11)     NOT NULL COMMENT '时间戳',
    PRIMARY KEY (`id`),
    KEY `m_time` (`m_time`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='玩法记录表';

-- --------------------------------------------------------

--
-- 新增表，主动放弃地块日志
-- daer
-- 2021070702
--

DROP TABLE IF EXISTS `log_tab_role_abandon_grid`;
CREATE TABLE IF NOT EXISTS `log_tab_role_abandon_grid`
(
    `id`         int(11) UNSIGNED    NOT NULL AUTO_INCREMENT,
    `agent_name` varchar(20)         NOT NULL COMMENT '代理名',
    `channel`    varchar(20)         NOT NULL COMMENT '渠道名',
    `role_id`    bigint(20) UNSIGNED NOT NULL COMMENT '角色id',
    `acc_name`   varchar(64)         NOT NULL COMMENT '账号名',
    `role_name`  varchar(20)         NOT NULL COMMENT '角色名',
    `grid_x`     int(11)             NOT NULL COMMENT '地块X坐标',
    `grid_y`     int(11)             NOT NULL COMMENT '地块Y坐标',
    `grid_level` tinyint(3)          NOT NULL COMMENT '地块等级',
    `m_time`     int(11) UNSIGNED    NOT NULL COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`),
    KEY `grid_x_grid_y` (`grid_x`, `grid_y`),
    KEY `m_time` (`m_time`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='主动放弃地块日志表';

-- --------------------------------------------------------

--
-- 添加表，禁言封号
-- kg
-- 2020083116
--

DROP TABLE IF EXISTS `log_tab_role_ban`;
CREATE TABLE IF NOT EXISTS `log_tab_role_ban`
(
    `id`       int(11)     NOT NULL AUTO_INCREMENT,
    `role_id`  bigint(20)  NOT NULL COMMENT '玩家id',
    `type`     int(11)     NOT NULL COMMENT 'type：2.封号 3.禁言',
    `acc_name` varchar(64) NOT NULL COMMENT '账号名',
    `time`     int(11)     NOT NULL COMMENT '封号禁言时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`),
    KEY `type` (`type`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='聊天举报';

-- --------------------------------------------------------

--
-- 新增表，技能孔
-- daer
-- 2021070703
--

DROP TABLE IF EXISTS `log_tab_role_camp_skill_hole`;
CREATE TABLE IF NOT EXISTS `log_tab_role_camp_skill_hole`
(
    `id`            int(11) UNSIGNED NOT NULL AUTO_INCREMENT,
    `channel`       varchar(20)      NOT NULL COMMENT '渠道名',
    `role_id`       bigint(20)       NOT NULL COMMENT '角色id',
    `acc_name`      varchar(64)      NOT NULL COMMENT '账户名',
    `role_name`     varchar(20)      NOT NULL COMMENT '角色名',
    `city_id`       bigint(20)       NOT NULL COMMENT '城池id',
    `city_type`     tinyint(4)       NOT NULL COMMENT '城池类型',
    `building_type` int(11)          NOT NULL COMMENT '建筑类型',
    `hole_index`    tinyint(4)       NOT NULL COMMENT '技能孔序号',
    `m_time`        int(11)          NOT NULL COMMENT '时间',
    PRIMARY KEY (`id`) USING BTREE,
    KEY `role_id` (`role_id`) USING BTREE,
    KEY `m_time` (`m_time`) USING BTREE
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4;

-- --------------------------------------------------------

--
-- 添加表，神秘卡日志
-- Tom
-- 2020102001
--

DROP TABLE IF EXISTS `log_tab_role_card`;
CREATE TABLE IF NOT EXISTS `log_tab_role_card`
(
    `id`             int(11) UNSIGNED    NOT NULL AUTO_INCREMENT,
    `role_id`        bigint(20) UNSIGNED NOT NULL COMMENT '角色id',
    `acc_name`       varchar(64)         NOT NULL COMMENT '账号名',
    `role_name`      varchar(20)         NOT NULL COMMENT '玩家名',
    `card_id`        int(11) UNSIGNED    NOT NULL COMMENT '神秘卡配置ID',
    `num`            int(11) UNSIGNED    NOT NULL COMMENT '神秘卡数量',
    `type`           int(11) UNSIGNED    NOT NULL COMMENT '神秘卡类型',
    `op_type`        int(11) UNSIGNED    NOT NULL COMMENT '操作类型，1获得，2消耗，3合成，4盖印完成',
    `way_id`         int(11) UNSIGNED    NOT NULL DEFAULT '0' COMMENT '途径ID，11开启x锦囊，12神秘卡合成，13天下大势，14战略任务，15攻城奖励，16邮件，17礼包码；21升星，22复制，23觉醒，24重置，25还原，26转换',
    `way_param`      varchar(1024)                DEFAULT NULL COMMENT '途径对应参数',
    `status`         tinyint(1) UNSIGNED NOT NULL COMMENT '神秘卡状态，0未解锁，1发布中，2可解锁，3已解锁',
    `result`         tinyint(1) UNSIGNED NOT NULL COMMENT '结果，0失败，1成功，2离线保存',
    `item_list`      varchar(2048)       NOT NULL DEFAULT '[]' COMMENT '获得的道具列表，[{item_id,num},...]',
    `cost_list`      varchar(2048)       NOT NULL DEFAULT '[]' COMMENT '消耗道具列表',
    `general_before` varchar(500)                 DEFAULT NULL COMMENT '操作前武将',
    `general_after`  varchar(500)                 DEFAULT NULL COMMENT '操作后武将',
    `m_time`         int(11) UNSIGNED    NOT NULL COMMENT '时间戳',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='神秘卡日志';

-- --------------------------------------------------------

--
-- 添加表，删号
-- kg
-- 2020120201
--

DROP TABLE IF EXISTS `log_tab_role_chat_report`;
CREATE TABLE IF NOT EXISTS `log_tab_role_chat_report`
(
    `role_id`        bigint(20)    NOT NULL COMMENT '被举报者角色id',
    `time`           int(11)       NOT NULL COMMENT '举报时间',
    `tab`            int(11)       NOT NULL COMMENT '聊天类型',
    `acc_name`       varchar(64)   NOT NULL COMMENT '被举报者账号名',
    `server_num`     int(11)       NOT NULL COMMENT '被举报者服务器id',
    `content`        varchar(5000) NOT NULL COMMENT '被举报者内容',
    `type_list`      int(11)       NOT NULL COMMENT '举报类型',
    `report_content` varchar(5000) DEFAULT NULL COMMENT '举报原因',
    `report_id`      bigint(20)    NOT NULL COMMENT '举报者id',
    PRIMARY KEY (`role_id`, `time`, `report_id`, `tab`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='聊天举报';

-- --------------------------------------------------------

--
-- 添加表，免战统计表
-- gjx
-- 2021042701
--

DROP TABLE IF EXISTS `log_tab_role_city_protect`;
CREATE TABLE IF NOT EXISTS `log_tab_role_city_protect`
(
    `id`        int(11) UNSIGNED    NOT NULL AUTO_INCREMENT,
    `role_id`   bigint(20) UNSIGNED NOT NULL COMMENT '角色id',
    `acc_name`  varchar(64)         NOT NULL COMMENT '账号名',
    `role_name` varchar(20)         NOT NULL COMMENT '玩家名',
    `city_name` varchar(20)         NOT NULL COMMENT '城池名字',
    `type`      int(11) UNSIGNED    NOT NULL COMMENT '免战类型 1:免战 2：取消免战',
    `reason`    int(11) UNSIGNED    NOT NULL COMMENT '免战原因',
    `m_time`    int(11) UNSIGNED    NOT NULL COMMENT '时间戳',
    PRIMARY KEY (`id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='免战统计日志';

-- --------------------------------------------------------

--
-- 添加表，删号
-- Karl
-- 2020070701
--

DROP TABLE IF EXISTS `log_tab_role_clean_up`;
CREATE TABLE IF NOT EXISTS `log_tab_role_clean_up`
(
    `id`         int(11) UNSIGNED     NOT NULL AUTO_INCREMENT,
    `agent_name` varchar(20)          NOT NULL COMMENT '代理名',
    `channel`    varchar(20)          NOT NULL COMMENT '渠道名',
    `acc_name`   varchar(64)          NOT NULL COMMENT '账号名',
    `role_id`    bigint(20) UNSIGNED  NOT NULL COMMENT '角色id',
    `role_level` smallint(5) UNSIGNED NOT NULL COMMENT '角色等级',
    `oper_type`  tinyint(3) UNSIGNED  NOT NULL COMMENT '操作类型 1-新手删号 2-进入非活跃 3-恢复活跃',
    `m_time`     int(11) UNSIGNED     NOT NULL COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`),
    KEY `m_time` (`m_time`),
    KEY `acc_name` (`acc_name`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='删号非活跃日志';

-- --------------------------------------------------------

--
-- 添加表，创角表
-- daer
-- 2017080201
--

DROP TABLE IF EXISTS `log_tab_role_create`;
CREATE TABLE IF NOT EXISTS `log_tab_role_create`
(
    `id`          int(11)     NOT NULL AUTO_INCREMENT,
    `agent_name`  varchar(20) NOT NULL COMMENT '代理名',
    `channel`     varchar(20) NOT NULL COMMENT '渠道名',
    `acc_name`    varchar(64) NOT NULL COMMENT '账号名',
    `role_id`     bigint(20)  NOT NULL COMMENT '角色id',
    `role_name`   varchar(20) NOT NULL COMMENT '玩家名',
    `country`     smallint(6) NOT NULL COMMENT '国家',
    `sex`         tinyint(4)  NOT NULL COMMENT '性别',
    `m_time`      int(11)     NOT NULL COMMENT '时间',
    `device_type` smallint(6) NOT NULL DEFAULT '0' COMMENT '客户端类型，1为ios, 2为安卓',
    PRIMARY KEY (`id`),
    KEY `m_time` (`m_time`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='创角表';

-- --------------------------------------------------------

--
-- 添加表，武将获得与分解
-- Karl
-- 2020060501
--

DROP TABLE IF EXISTS `log_tab_role_general`;
CREATE TABLE IF NOT EXISTS `log_tab_role_general`
(
    `id`                int(11) UNSIGNED     NOT NULL AUTO_INCREMENT,
    `agent_name`        varchar(20)          NOT NULL COMMENT '代理名',
    `channel`           varchar(20)          NOT NULL COMMENT '渠道名',
    `role_id`           bigint(20) UNSIGNED  NOT NULL COMMENT '角色id',
    `general_type`      int(11)              NOT NULL COMMENT '武将类型，对应武将表',
    `general_color`     tinyint(3)           NOT NULL COMMENT '武将颜色',
    `general_id`        bigint(20) UNSIGNED  NOT NULL COMMENT '武将id',
    `general_lv`        smallint(5) UNSIGNED NOT NULL COMMENT '武将等级',
    `exp`               int(11) UNSIGNED     NOT NULL COMMENT '武将经验',
    `general_star`      tinyint(3) UNSIGNED  NOT NULL COMMENT '武将星级',
    `awaken`            tinyint(3) UNSIGNED  NOT NULL COMMENT '武将觉醒等级',
    `num`               int(3) UNSIGNED      NOT NULL COMMENT '数量',
    `step`              int(3) UNSIGNED      NOT NULL DEFAULT '0' COMMENT '阶级',
    `point_list`        varchar(1024)                 DEFAULT NULL COMMENT '途径参数',
    `oper_type`         tinyint(3) UNSIGNED  NOT NULL COMMENT '操作类型 0-获得 1-消耗 2-升星 3-觉醒 4-升级 5-升阶 6-加点 7-洗点',
    `oper_way`          smallint(5) UNSIGNED NOT NULL COMMENT '操作途径，1自动分解，2手动分解，3拆解技能，4研究技能，5升星，6觉醒，7转换， 8升级 11x道具获得，12招募获得，13转换卡获得，14首充x元获得，15累充x元获得，16每日精选x元宝购买获得，17x活动获得，18邮件获得，19礼包码获得 20GM获得 21复制获得 22还原卡获得',
    `way_param`         varchar(1024)                 DEFAULT NULL COMMENT '途径参数',
    `item_list`         mediumtext COMMENT '获得道具列表，json列表',
    `card_cost_list`    mediumtext COMMENT '神秘卡消耗列表，json列表',
    `general_cost_list` mediumtext COMMENT '武将消耗列表，json列表',
    `item_cost_list`    mediumtext COMMENT '道具消耗列表，json列表',
    `m_time`            int(10) UNSIGNED     NOT NULL COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`),
    KEY `m_time` (`m_time`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='武将获得与分解';

-- --------------------------------------------------------

--
-- 添加表，道具表
-- zsk
-- 2018120402
--

DROP TABLE IF EXISTS `log_tab_role_goods`;
CREATE TABLE IF NOT EXISTS `log_tab_role_goods`
(
    `id`         int(11)      NOT NULL AUTO_INCREMENT,
    `role_id`    bigint(20)   NOT NULL COMMENT '角色id',
    `acc_name`   varchar(64)  NOT NULL COMMENT '账号名',
    `role_name`  varchar(20)  NOT NULL COMMENT '玩家名',
    `goods_type` mediumint(9) NOT NULL COMMENT '物品类型（资源类型）',
    `goods_num`  int(11)      NOT NULL COMMENT '道具数量',
    `goods_name` varchar(50)  NOT NULL COMMENT '道具名',
    `type`       int(11)      NOT NULL COMMENT '消耗/获得(0是获得，1是消耗)',
    `way`        int(11)      NOT NULL COMMENT '途径',
    `m_time`     int(11)      NOT NULL COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `m_time` (`m_time`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='道具表';

-- --------------------------------------------------------

--
-- 添加表，道具消耗分表，主动放弃地块表，部分表添加索引
-- Karl
-- 2020083101
--

DROP TABLE IF EXISTS `log_tab_role_goods_0`;
CREATE TABLE IF NOT EXISTS `log_tab_role_goods_0`
(
    `id`         int(11) UNSIGNED NOT NULL AUTO_INCREMENT,
    `role_id`    bigint(20)       NOT NULL COMMENT '角色id',
    `acc_name`   varchar(64)      NOT NULL COMMENT '账号名',
    `role_name`  varchar(20)      NOT NULL COMMENT '玩家名',
    `goods_type` mediumint(9)     NOT NULL COMMENT '物品类型（资源类型）',
    `goods_num`  int(11)          NOT NULL COMMENT '道具数量',
    `goods_name` varchar(50)      NOT NULL COMMENT '道具名',
    `type`       int(11)          NOT NULL COMMENT '消耗/获得(0是获得，1是消耗)',
    `way`        int(11)          NOT NULL COMMENT '途径',
    `m_time`     int(11)          NOT NULL COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`),
    KEY `m_time` (`m_time`),
    KEY `goods_type` (`goods_type`) USING BTREE
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='道具消耗分表0';

-- --------------------------------------------------------

--
-- 添加表，道具消耗分表1
-- Karl
-- 2020083102
--

DROP TABLE IF EXISTS `log_tab_role_goods_1`;
CREATE TABLE IF NOT EXISTS `log_tab_role_goods_1`
(
    `id`         int(11) UNSIGNED NOT NULL AUTO_INCREMENT,
    `role_id`    bigint(20)       NOT NULL COMMENT '角色id',
    `acc_name`   varchar(64)      NOT NULL COMMENT '账号名',
    `role_name`  varchar(20)      NOT NULL COMMENT '玩家名',
    `goods_type` mediumint(9)     NOT NULL COMMENT '物品类型（资源类型）',
    `goods_num`  int(11)          NOT NULL COMMENT '道具数量',
    `goods_name` varchar(50)      NOT NULL COMMENT '道具名',
    `type`       int(11)          NOT NULL COMMENT '消耗/获得(0是获得，1是消耗)',
    `way`        int(11)          NOT NULL COMMENT '途径',
    `m_time`     int(11)          NOT NULL COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`),
    KEY `m_time` (`m_time`),
    KEY `goods_type` (`goods_type`) USING BTREE
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='道具消耗分表1';

-- --------------------------------------------------------

--
-- 添加表，道具消耗分表2
-- Karl
-- 2020083103
--

DROP TABLE IF EXISTS `log_tab_role_goods_2`;
CREATE TABLE IF NOT EXISTS `log_tab_role_goods_2`
(
    `id`         int(11) UNSIGNED NOT NULL AUTO_INCREMENT,
    `role_id`    bigint(20)       NOT NULL COMMENT '角色id',
    `acc_name`   varchar(64)      NOT NULL COMMENT '账号名',
    `role_name`  varchar(20)      NOT NULL COMMENT '玩家名',
    `goods_type` mediumint(9)     NOT NULL COMMENT '物品类型（资源类型）',
    `goods_num`  int(11)          NOT NULL COMMENT '道具数量',
    `goods_name` varchar(50)      NOT NULL COMMENT '道具名',
    `type`       int(11)          NOT NULL COMMENT '消耗/获得(0是获得，1是消耗)',
    `way`        int(11)          NOT NULL COMMENT '途径',
    `m_time`     int(11)          NOT NULL COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`),
    KEY `m_time` (`m_time`),
    KEY `goods_type` (`goods_type`) USING BTREE
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='道具消耗分表2';

-- --------------------------------------------------------

--
-- 添加表，道具消耗分表3
-- Karl
-- 2020083104
--

DROP TABLE IF EXISTS `log_tab_role_goods_3`;
CREATE TABLE IF NOT EXISTS `log_tab_role_goods_3`
(
    `id`         int(11) UNSIGNED NOT NULL AUTO_INCREMENT,
    `role_id`    bigint(20)       NOT NULL COMMENT '角色id',
    `acc_name`   varchar(64)      NOT NULL COMMENT '账号名',
    `role_name`  varchar(20)      NOT NULL COMMENT '玩家名',
    `goods_type` mediumint(9)     NOT NULL COMMENT '物品类型（资源类型）',
    `goods_num`  int(11)          NOT NULL COMMENT '道具数量',
    `goods_name` varchar(50)      NOT NULL COMMENT '道具名',
    `type`       int(11)          NOT NULL COMMENT '消耗/获得(0是获得，1是消耗)',
    `way`        int(11)          NOT NULL COMMENT '途径',
    `m_time`     int(11)          NOT NULL COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`),
    KEY `m_time` (`m_time`),
    KEY `goods_type` (`goods_type`) USING BTREE
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='道具消耗分表3';

-- --------------------------------------------------------

--
-- 添加表，道具消耗分表4
-- Karl
-- 2020083105
--

DROP TABLE IF EXISTS `log_tab_role_goods_4`;
CREATE TABLE IF NOT EXISTS `log_tab_role_goods_4`
(
    `id`         int(11) UNSIGNED NOT NULL AUTO_INCREMENT,
    `role_id`    bigint(20)       NOT NULL COMMENT '角色id',
    `acc_name`   varchar(64)      NOT NULL COMMENT '账号名',
    `role_name`  varchar(20)      NOT NULL COMMENT '玩家名',
    `goods_type` mediumint(9)     NOT NULL COMMENT '物品类型（资源类型）',
    `goods_num`  int(11)          NOT NULL COMMENT '道具数量',
    `goods_name` varchar(50)      NOT NULL COMMENT '道具名',
    `type`       int(11)          NOT NULL COMMENT '消耗/获得(0是获得，1是消耗)',
    `way`        int(11)          NOT NULL COMMENT '途径',
    `m_time`     int(11)          NOT NULL COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`),
    KEY `m_time` (`m_time`),
    KEY `goods_type` (`goods_type`) USING BTREE
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='道具消耗分表4';

-- --------------------------------------------------------

--
-- 添加表，道具消耗分表5
-- Karl
-- 2020083106
--

DROP TABLE IF EXISTS `log_tab_role_goods_5`;
CREATE TABLE IF NOT EXISTS `log_tab_role_goods_5`
(
    `id`         int(11) UNSIGNED NOT NULL AUTO_INCREMENT,
    `role_id`    bigint(20)       NOT NULL COMMENT '角色id',
    `acc_name`   varchar(64)      NOT NULL COMMENT '账号名',
    `role_name`  varchar(20)      NOT NULL COMMENT '玩家名',
    `goods_type` mediumint(9)     NOT NULL COMMENT '物品类型（资源类型）',
    `goods_num`  int(11)          NOT NULL COMMENT '道具数量',
    `goods_name` varchar(50)      NOT NULL COMMENT '道具名',
    `type`       int(11)          NOT NULL COMMENT '消耗/获得(0是获得，1是消耗)',
    `way`        int(11)          NOT NULL COMMENT '途径',
    `m_time`     int(11)          NOT NULL COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`),
    KEY `m_time` (`m_time`),
    KEY `goods_type` (`goods_type`) USING BTREE
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='道具消耗分表5';

-- --------------------------------------------------------

--
-- 添加表，道具消耗分表6
-- Karl
-- 2020083107
--

DROP TABLE IF EXISTS `log_tab_role_goods_6`;
CREATE TABLE IF NOT EXISTS `log_tab_role_goods_6`
(
    `id`         int(11) UNSIGNED NOT NULL AUTO_INCREMENT,
    `role_id`    bigint(20)       NOT NULL COMMENT '角色id',
    `acc_name`   varchar(64)      NOT NULL COMMENT '账号名',
    `role_name`  varchar(20)      NOT NULL COMMENT '玩家名',
    `goods_type` mediumint(9)     NOT NULL COMMENT '物品类型（资源类型）',
    `goods_num`  int(11)          NOT NULL COMMENT '道具数量',
    `goods_name` varchar(50)      NOT NULL COMMENT '道具名',
    `type`       int(11)          NOT NULL COMMENT '消耗/获得(0是获得，1是消耗)',
    `way`        int(11)          NOT NULL COMMENT '途径',
    `m_time`     int(11)          NOT NULL COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`),
    KEY `m_time` (`m_time`),
    KEY `goods_type` (`goods_type`) USING BTREE
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='道具消耗分表6';

-- --------------------------------------------------------

--
-- 添加表，道具消耗分表7
-- Karl
-- 2020083108
--

DROP TABLE IF EXISTS `log_tab_role_goods_7`;
CREATE TABLE IF NOT EXISTS `log_tab_role_goods_7`
(
    `id`         int(11) UNSIGNED NOT NULL AUTO_INCREMENT,
    `role_id`    bigint(20)       NOT NULL COMMENT '角色id',
    `acc_name`   varchar(64)      NOT NULL COMMENT '账号名',
    `role_name`  varchar(20)      NOT NULL COMMENT '玩家名',
    `goods_type` mediumint(9)     NOT NULL COMMENT '物品类型（资源类型）',
    `goods_num`  int(11)          NOT NULL COMMENT '道具数量',
    `goods_name` varchar(50)      NOT NULL COMMENT '道具名',
    `type`       int(11)          NOT NULL COMMENT '消耗/获得(0是获得，1是消耗)',
    `way`        int(11)          NOT NULL COMMENT '途径',
    `m_time`     int(11)          NOT NULL COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`),
    KEY `m_time` (`m_time`),
    KEY `goods_type` (`goods_type`) USING BTREE
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='道具消耗分表7';

-- --------------------------------------------------------

--
-- 添加表，玩家引导信息记录表
-- hyq
-- 2019061701
--

DROP TABLE IF EXISTS `log_tab_role_guide_step`;
CREATE TABLE IF NOT EXISTS `log_tab_role_guide_step`
(
    `id`         int(11)     NOT NULL AUTO_INCREMENT,
    `agent_name` varchar(20) NOT NULL COMMENT '代理名',
    `channel`    varchar(20) NOT NULL COMMENT '渠道名',
    `acc_name`   varchar(64) NOT NULL COMMENT '账号名',
    `role_id`    bigint(20)  NOT NULL COMMENT '角色id',
    `role_name`  varchar(20) NOT NULL COMMENT '玩家名',
    `step`       int(11)     NOT NULL COMMENT '引导阶段',
    `m_time`     int(11)     NOT NULL COMMENT '时间戳',
    PRIMARY KEY (`id`),
    KEY `m_time` (`m_time`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='玩家引导信息记录表';

-- --------------------------------------------------------

--
-- 添加表，等级表
-- zsk
-- 2018121102
--

DROP TABLE IF EXISTS `log_tab_role_lev`;
CREATE TABLE IF NOT EXISTS `log_tab_role_lev`
(
    `id`         int(11)      NOT NULL AUTO_INCREMENT,
    `agent_name` varchar(20)  NOT NULL COMMENT '代理名',
    `channel`    varchar(20)  NOT NULL COMMENT '渠道名',
    `role_id`    bigint(20)   NOT NULL COMMENT '角色id',
    `acc_name`   varchar(64)  NOT NULL COMMENT '账号名',
    `role_name`  varchar(20)  NOT NULL COMMENT '玩家名',
    `lev`        mediumint(9) NOT NULL COMMENT '等级',
    `m_time`     int(11)      NOT NULL COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `m_time` (`m_time`),
    KEY `role_id` (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='等级表';

-- --------------------------------------------------------

--
-- 添加表，登录表
-- daer
-- 2017080202
--

DROP TABLE IF EXISTS `log_tab_role_login`;
CREATE TABLE IF NOT EXISTS `log_tab_role_login`
(
    `id`          int(11)      NOT NULL AUTO_INCREMENT,
    `agent_name`  varchar(20)  NOT NULL COMMENT '代理名',
    `channel`     varchar(20)  NOT NULL COMMENT '渠道名',
    `role_id`     bigint(20)   NOT NULL COMMENT '角色id',
    `acc_name`    varchar(64)  NOT NULL COMMENT '账号名',
    `role_name`   varchar(20)  NOT NULL COMMENT '玩家名',
    `level`       mediumint(9) NOT NULL COMMENT '等级',
    `device_id`   varchar(100) NOT NULL COMMENT '登录设备id',
    `ip`          varchar(30)  NOT NULL COMMENT 'ip地址',
    `create_time` int(11)      NOT NULL COMMENT '创建时间',
    `m_time`      int(11)      NOT NULL COMMENT '时间',
    `country`     smallint(6)  NOT NULL COMMENT '国家',
    `device_type` smallint(6)  NOT NULL DEFAULT '0' COMMENT '客户端类型，1为ios, 2为安卓',
    PRIMARY KEY (`id`),
    KEY `m_time` (`m_time`),
    KEY `role_id` (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='登录表';

-- --------------------------------------------------------

--
-- 添加表，下线表
-- daer
-- 2017080203
--

DROP TABLE IF EXISTS `log_tab_role_logout`;
CREATE TABLE IF NOT EXISTS `log_tab_role_logout`
(
    `id`          int(11)      NOT NULL AUTO_INCREMENT,
    `agent_name`  varchar(20)  NOT NULL COMMENT '代理名',
    `channel`     varchar(20)  NOT NULL COMMENT '渠道名',
    `role_id`     bigint(20)   NOT NULL COMMENT '角色id',
    `acc_name`    varchar(64)  NOT NULL COMMENT '账号名',
    `role_name`   varchar(20)  NOT NULL COMMENT '玩家名',
    `level`       mediumint(9) NOT NULL COMMENT '等级',
    `login_time`  int(11)      NOT NULL COMMENT '登录时间',
    `create_time` int(11)      NOT NULL COMMENT '创建时间',
    `reason`      int(11)      NOT NULL COMMENT '下线原因',
    `m_time`      int(11)      NOT NULL COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `m_time` (`m_time`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='下线表';

-- --------------------------------------------------------

--
-- 添加表，返利表
-- hyq
-- 2020082401
--

DROP TABLE IF EXISTS `log_tab_role_rebate`;
CREATE TABLE IF NOT EXISTS `log_tab_role_rebate`
(
    `id`           int(11)      NOT NULL AUTO_INCREMENT,
    `agent_name`   varchar(20)  NOT NULL COMMENT '代理名',
    `channel`      varchar(20)  NOT NULL COMMENT '渠道名',
    `acc_name`     varchar(64)  NOT NULL COMMENT '账号名',
    `server_num`   int(11)      NOT NULL COMMENT '服务器id',
    `role_id`      bigint(20)   NOT NULL COMMENT '角色id',
    `money`        mediumint(9) NOT NULL COMMENT '充值金额',
    `rebate_level` mediumint(9) NOT NULL COMMENT '返利等级',
    `reward_list`  blob COMMENT '奖励',
    `m_time`       int(11)      NOT NULL COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `m_time` (`m_time`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='返利表';

-- --------------------------------------------------------

--
-- 添加表，充值表
-- zsk
-- 2018120602
--

DROP TABLE IF EXISTS `log_tab_role_recharge`;
CREATE TABLE IF NOT EXISTS `log_tab_role_recharge`
(
    `id`          int(11)      NOT NULL AUTO_INCREMENT,
    `agent_name`  varchar(20)  NOT NULL COMMENT '代理名',
    `channel`     varchar(20)  NOT NULL COMMENT '渠道名',
    `role_id`     bigint(20)   NOT NULL COMMENT '角色id',
    `acc_name`    varchar(64)  NOT NULL COMMENT '账号名',
    `role_name`   varchar(20)  NOT NULL COMMENT '玩家名',
    `lev`         mediumint(9) NOT NULL COMMENT '玩家等级',
    `recharge_id` mediumint(9) NOT NULL COMMENT '充值id',
    `money`       mediumint(9) NOT NULL COMMENT '充值金额',
    `m_time`      int(11)      NOT NULL COMMENT '时间',
    `country`     smallint(6)  NOT NULL COMMENT '国家',
    `device_type` smallint(6)  NOT NULL DEFAULT '0' COMMENT '客户端类型，1为ios, 2为安卓',
    PRIMARY KEY (`id`),
    KEY `m_time` (`m_time`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='充值表';

-- --------------------------------------------------------

--
-- 添加表 招募日志
-- daer
-- 2021070704
--

DROP TABLE IF EXISTS `log_tab_role_recruit`;
CREATE TABLE IF NOT EXISTS `log_tab_role_recruit`
(
    `id`              int(11) UNSIGNED NOT NULL AUTO_INCREMENT,
    `role_id`         bigint(20)       NOT NULL COMMENT '角色id',
    `role_name`       varchar(20)      NOT NULL COMMENT '角色名',
    `acc_name`        varchar(64)      NOT NULL COMMENT '账号名',
    `recruit_id`      int(11)          NOT NULL COMMENT '本次操作的卡池ID',
    `recruit_only_id` bigint(20)       NOT NULL DEFAULT '0' COMMENT '卡池唯一ID',
    `cost`            blob COMMENT '本次消耗',
    `generals`        blob COMMENT '本次获得的武将',
    `time`            int(11) UNSIGNED NOT NULL COMMENT '招募时间',
    `server_num`      int(11) UNSIGNED NOT NULL COMMENT '服务器id',
    `recruit_time`    int(11)          NOT NULL DEFAULT '0' COMMENT '招募次数',
    `cost_type`       int(11)          NOT NULL DEFAULT '0' COMMENT '消耗类型1:银元宝2:道具3:免费4:刀币',
    `cost_num`        int(11)          NOT NULL DEFAULT '0' COMMENT '消耗数量',
    PRIMARY KEY (`id`),
    KEY `role_id` (`role_id`),
    KEY `recruit_only_id` (`recruit_only_id`) USING BTREE
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='招募日志';

-- --------------------------------------------------------

--
-- 添加表，玩家个人资源每日汇总
-- zsk
-- 2019010708
--
DROP TABLE IF EXISTS `log_tab_role_res`;
CREATE TABLE IF NOT EXISTS `log_tab_role_res`
(
    `id`                 int(11)     NOT NULL AUTO_INCREMENT,
    `agent_name`         varchar(20) NOT NULL COMMENT '代理名',
    `acc_name`           varchar(64) NOT NULL COMMENT '账号名',
    `role_id`            bigint(20)  NOT NULL COMMENT '角色id',
    `name`               varchar(20) NOT NULL COMMENT '玩家名',
    `wood`               int(11)     NOT NULL COMMENT '木头',
    `food`               int(11)     NOT NULL COMMENT '粮食',
    `rock`               int(11)     NOT NULL COMMENT '石料',
    `iron`               int(11)     NOT NULL COMMENT '铁矿',
    `coin`               int(11)     NOT NULL COMMENT '金币',
    `diamond`            int(11)     NOT NULL COMMENT '元宝',
    `donation`           int(11)     NOT NULL COMMENT '公会贡献',
    `ru`                 int(11)     NOT NULL COMMENT '儒家声望',
    `dao`                int(11)     NOT NULL COMMENT '道家声望',
    `fa`                 int(11)     NOT NULL COMMENT '法家声望',
    `bing`               int(11)     NOT NULL COMMENT '兵家声望',
    `mo`                 int(11)     NOT NULL COMMENT '墨家声望',
    `qun`                int(11)     NOT NULL COMMENT '群贤声望',
    `wood_city_out_put`  int(11)     NOT NULL COMMENT '木头城内产出',
    `wood_field_out_put` int(11)     NOT NULL COMMENT '木头城外产出',
    `food_city_out_put`  int(11)     NOT NULL COMMENT '粮食城内产出',
    `food_field_out_put` int(11)     NOT NULL COMMENT '粮食城外产出',
    `iron_city_out_put`  int(11)     NOT NULL COMMENT '铁城内产出',
    `iron_field_out_put` int(11)     NOT NULL COMMENT '铁城外产出',
    `rock_city_out_put`  int(11)     NOT NULL COMMENT '石头城内产出',
    `rock_field_out_put` int(11)     NOT NULL COMMENT '石头城外产出',
    `m_time`             int(11)     NOT NULL COMMENT '时间戳',
    PRIMARY KEY (`id`),
    KEY `m_time` (`m_time`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='玩家个人资源每日汇总';

-- --------------------------------------------------------

--
-- 添加表，商店表
-- hyq
-- 2019071601
--

DROP TABLE IF EXISTS `log_tab_role_shop`;
CREATE TABLE IF NOT EXISTS `log_tab_role_shop`
(
    `id`         int(11)      NOT NULL AUTO_INCREMENT,
    `agent_name` varchar(20)  NOT NULL COMMENT '代理名',
    `channel`    varchar(20)  NOT NULL COMMENT '渠道名',
    `role_id`    bigint(20)   NOT NULL COMMENT '角色id',
    `shop_type`  int(11)      NOT NULL COMMENT '商店类型',
    `goods_type` mediumint(9) NOT NULL COMMENT '物品类型（资源类型）',
    `goods_num`  int(11)      NOT NULL COMMENT '道具数量',
    `goods_name` varchar(20)  NOT NULL COMMENT '道具名',
    `m_time`     int(11)      NOT NULL COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `m_time` (`m_time`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='道具表';

-- --------------------------------------------------------

--
-- 添加表，玩家士兵损失记录表
-- zsk
-- 2019010709
--

DROP TABLE IF EXISTS `log_tab_role_soldier_death`;
CREATE TABLE IF NOT EXISTS `log_tab_role_soldier_death`
(
    `id`          int(11)     NOT NULL AUTO_INCREMENT,
    `agent_name`  varchar(20) NOT NULL COMMENT '代理名',
    `role_id`     bigint(20)  NOT NULL COMMENT '角色id',
    `soldier_num` int(11)     NOT NULL COMMENT '损失士兵数量',
    `m_time`      int(11)     NOT NULL COMMENT '时间戳',
    PRIMARY KEY (`id`),
    KEY `m_time` (`m_time`),
    KEY `role_id` (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='玩家士兵损失记录表';

-- --------------------------------------------------------

--
-- 添加表，任务表
-- zsk
-- 2018120502
--

DROP TABLE IF EXISTS `log_tab_role_task`;
CREATE TABLE IF NOT EXISTS `log_tab_role_task`
(
    `id`             int(11)      NOT NULL AUTO_INCREMENT,
    `role_id`        bigint(20)   NOT NULL COMMENT '角色id',
    `acc_name`       varchar(64)  NOT NULL COMMENT '账号名',
    `role_name`      varchar(20)  NOT NULL COMMENT '玩家名',
    `task_config_id` mediumint(9) NOT NULL COMMENT '任务配置id',
    `task_name`      varchar(20)  NOT NULL COMMENT '任务名字',
    `state`          int(11)      NOT NULL COMMENT '任务状态',
    `m_time`         int(11)      NOT NULL COMMENT '时间',
    PRIMARY KEY (`id`),
    KEY `m_time` (`m_time`),
    KEY `role_id` (`role_id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='任务表';

-- --------------------------------------------------------

--
-- 新增锦囊日志表
-- Tom
-- 2020102201
--

DROP TABLE IF EXISTS `log_tab_role_time_box`;
CREATE TABLE IF NOT EXISTS `log_tab_role_time_box`
(
    `id`        int(11) UNSIGNED    NOT NULL AUTO_INCREMENT,
    `role_id`   bigint(20) UNSIGNED NOT NULL COMMENT '角色id',
    `acc_name`  varchar(64)         NOT NULL COMMENT '账号名',
    `role_name` varchar(20)         NOT NULL COMMENT '玩家名',
    `type`      int(11) UNSIGNED    NOT NULL COMMENT '锦囊类型',
    `op_type`   int(11) UNSIGNED    NOT NULL COMMENT '操作类型，1消耗',
    `way_id`    int(11) UNSIGNED    NOT NULL COMMENT '途径ID，11丢弃，12暂存过期，13x元宝开启，14时间开启',
    `way_param` varchar(1024)       NOT NULL COMMENT '途径对应参数',
    `item_list` varchar(2048)       NOT NULL DEFAULT '[]' COMMENT '获得物品，[{item_id,num},...]',
    `m_time`    int(11) UNSIGNED    NOT NULL COMMENT '时间戳',
    PRIMARY KEY (`id`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='锦囊日志';

-- --------------------------------------------------------

--
-- 添加表，六爻起卦表
-- daer
-- 2021070705
--

DROP TABLE IF EXISTS `log_tab_server_divination`;
CREATE TABLE IF NOT EXISTS `log_tab_server_divination`
(
    `id`               int(11) UNSIGNED NOT NULL AUTO_INCREMENT,
    `open_time`        int(11) UNSIGNED NOT NULL COMMENT '开启时间',
    `divination_times` int(11) UNSIGNED NOT NULL COMMENT '卜卦总次数',
    `role_id`          bigint(20)       NOT NULL COMMENT '一等奖角色id',
    `role_name`        varchar(20)      NOT NULL COMMENT '一等奖角色名',
    `reward_list`      blob COMMENT '一等奖奖励列表',
    `prize_times_list` blob COMMENT '奖励次数列表',
    `m_time`           int(11) UNSIGNED NOT NULL COMMENT '时间戳',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4;

-- --------------------------------------------------------

--
-- 添加表，限时活动记录表
-- zsk
-- 2019010710
--

DROP TABLE IF EXISTS `log_tab_time_activity`;
CREATE TABLE IF NOT EXISTS `log_tab_time_activity`
(
    `id`            int(11)     NOT NULL AUTO_INCREMENT,
    `agent_name`    varchar(20) NOT NULL COMMENT '渠道名',
    `role_id`       bigint(20)  NOT NULL COMMENT '角色id',
    `activity_type` int(11)     NOT NULL COMMENT '活动类型',
    `m_time`        int(11)     NOT NULL COMMENT '时间戳',
    PRIMARY KEY (`id`),
    KEY `m_time` (`m_time`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='活动记录表';

-- --------------------------------------------------------

--
-- 添加表，联盟活跃记录表
-- zsk
-- 2019010711
--

DROP TABLE IF EXISTS `log_tab_union_activity_point`;
CREATE TABLE IF NOT EXISTS `log_tab_union_activity_point`
(
    `id`           int(11)     NOT NULL AUTO_INCREMENT,
    `agent_name`   varchar(20) NOT NULL COMMENT '代理名',
    `union_id`     bigint(20)  NOT NULL COMMENT '联盟id',
    `activity_num` int(11)     NOT NULL COMMENT '活跃人数',
    `other_num`    int(11)     NOT NULL COMMENT '不活跃人数',
    `m_time`       int(11)     NOT NULL COMMENT '时间戳',
    PRIMARY KEY (`id`),
    KEY `m_time` (`m_time`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4 COMMENT ='联盟活跃记录表';
COMMIT;
