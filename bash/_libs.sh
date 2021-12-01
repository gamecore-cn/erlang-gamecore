# shellcheck disable=SC2086 disable=SC2006 disable=SC2045 disable=SC2046 disable=SC2005  disable=SC2002 disable=SC2126 disable=SC2009 disable=SC2034 disable=SC2086
# 下面是字体输出颜色及终端格式控制
# 字体色范围：30-37
#   echo -e "\033[30m 黑色字 \033[0m"
#   echo -e "\033[31m 红色字 \033[0m"
#   echo -e "\033[32m 绿色字 \033[0m"
#   echo -e "\033[33m 黄色字 \033[0m"
#   echo -e "\033[34m 蓝色字 \033[0m"
#   echo -e "\033[35m 紫色字 \033[0m"
#   echo -e "\033[36m 天蓝字 \033[0m"
#   echo -e "\033[37m 白色字 \033[0m"
cc()
{
	_Color_=$1
	_Text_=$2
	echo -e "\033[${_Color_}m${_Text_}\033[0m"
}

# 目录拷贝
cp2()
{
	_Dir_Origin_=$1
	_Dir_Target_=$2
	rm -rf ${_Dir_Target_}
	check_dir ${_Dir_Target_}
	cc 33 "cp -rfp ${_Dir_Origin_} ${_Dir_Target_}"
	cp -rfp ${_Dir_Origin_} ${_Dir_Target_}
}

# 文件拷贝
cp3()
{
	_Dir_Origin_=$1
	_Dir_Target_=$2
	rm -rf ${_Dir_Target_}
	cc 33 "cp -rfp ${_Dir_Origin_} ${_Dir_Target_}"
	cp -rfp ${_Dir_Origin_} ${_Dir_Target_}
}

# 检查是否编译成功
check_status()
{
    _Msg_=$2
    _Time_="`date +\"%Y-%m-%d %H:%M:%S\"`"
    if test $1 != 0 ; then
        cc 31 "Time  :${_Time_}"
        cc 31 "failed:${_Msg_}"
        exit 1
    fi
}

# 不能重复执行
check_running()
{
	_Script_=$1
	_Pid_=$2
	_Time_="`date +\"%Y-%m-%d %H:%M:%S\"`"
	_Run_=`ps -ef | grep "/bin/bash /data/mingri/server_test_auto/sh/compile_test.sh" | grep -v grep | wc -l`
	#echo "run:${_Run_}"
	if [ ${_Run_} -gt 3 ]; then
	    if [ "${_Run_}" != "root" ]; then
            cc 31 "Time  :${_Time_}"
            cc 31 "Script:${_Script_} has running... "
            exit 1
		fi
	fi
}

# 目录
check_dir()
{
	_Dir_Name_=$1
	echo "check_dir:${_Dir_Name_}"
	if [ ! -d ${_Dir_Name_} ] ; then
		if [ ! -f ${_Dir_Name_} ] ; then
		    mkdir -p ${_Dir_Name_}
		else
		    cc 31 "${_Dir_Name_} is file ..."
		    exit 1
		fi
	fi
}

# 处理配置的编译时间
compile_time()
{
	__File__Name_=$1
	_Is_Test_=$2
	# 时间
	if test "${_Is_Test_}" -eq "1" ; then
		_Compile_Time_=""
	else
		_Compile_Time_="`date +\"{compile_time, {{%Y, %m, %d}, {%H, %M, %S}}},\"`"
	fi
	# 替换
    sed -i -r "s/\{compile_time.*$/${_Compile_Time_}/g" "${__File__Name_}"
}

# 编译代码
compile_code()
{
    # 参数 -----------------------
	_Dir_Root_=$1
	_Is_Test_=$2
	_Step_=$3
	# first_srcdir
	if test "$4" == ""; then
		_Src_Dir_=""
	else
		_Src_Dir_="first_srcdir="$4
	fi
	# first_ourdir
	if test "$5" == ""; then
		_Out_Dir_=""
	else
		_Out_Dir_="first_ourdir="$5
	fi
	# except_path
	if test "$6" == ""; then
		_Except_Dir_=""
	else
		_Except_Dir_="except_path="$6
	fi

	# 执行 -----------------------
	echo ""
	echo ""
	cc 32 "----------------------------------------------------------"
	cc 32 "${_Step_}. 编译代码 ...                                          |"
	cc 32 "----------------------------------------------------------"
	cc 32 "Dir_Root:${_Dir_Root_}  Is_Test:${_Is_Test_}"
	# 配制
	check_dir "${_Dir_Root_}ebin/game/"
	compile_time "${_Dir_Root_}rebar_config/rebar.config" ${_Is_Test_}
	# rebar
	chmod a+x "${_Dir_Root_}rebar"

    # echo "${_Dir_Root_}rebar co -j 8 --config ${_Dir_Root_}rebar_config/rebar.config  ${_Src_Dir_} ${_Out_Dir_} ${_Except_Dir_}"
	${_Dir_Root_}rebar co -j 8 --config "${_Dir_Root_}rebar_config/rebar.config"  ${_Src_Dir_} ${_Out_Dir_} ${_Except_Dir_}
	check_status $? "编译game代码出错error"
}

# 编译spec
compile_spec()
{
	_Dir_Root_=$1
	_Is_Test_=$2
	_Step_=$3
	echo ""
	echo ""
	cc 32 "----------------------------------------------------------"
	cc 32 "${_Step_}. 编译spec ...                                          |"
	cc 32 "----------------------------------------------------------"
	cc 32 "Dir_Root:${_Dir_Root_}  Is_Test:${_Is_Test_}"
	# 配制
	check_dir ${_Dir_Root_}ebin/spec/
	compile_time ${_Dir_Root_}rebar_config/rebar_spec.config ${_Is_Test_}
	# rebar
	chmod a+x ${_Dir_Root_}rebar
	${_Dir_Root_}rebar co -j 8 --config ${_Dir_Root_}rebar_config/rebar_spec.config
	check_status $? "编译spec出错error"
}

# 编译大地图配置 config_data
compile_data_map()
{
	_Dir_Root_=$1
	_Is_Test_=$2
	_Season_Id_=$3
	_Step_=$4
	echo ""
	echo ""
	cc 32 "----------------------------------------------------------"
	cc 32 "${_Step_}. 编译大地图数据...                                     |"
	cc 32 "----------------------------------------------------------"
	cc 32 "Dir_Root:${_Dir_Root_}  Is_Test:${_Is_Test_}  Season_Id:${_Season_Id_}"
	# 配制
	check_dir ${_Dir_Root_}ebin/config/s${_Season_Id_}/
	compile_time ${_Dir_Root_}rebar_config/rebar_map.config ${_Is_Test_}
	cp -rfp ${_Dir_Root_}config_data/block.bin ${_Dir_Root_}ebin/config/s${_Season_Id_}/
	# rebar
	chmod a+x ${_Dir_Root_}rebar
	${_Dir_Root_}rebar co -j 8 --config ${_Dir_Root_}rebar_config/rebar_map.config  first_outdir=ebin/config/s${_Season_Id_}
	check_status $? "编译大地图配置出错error"
}

# 编译赛季的内容
compile_data_season()
{
	_Dir_Root_=$1
	_Is_Test_=$2
	_Season_Id_=$3
	_Season_Id_Src_=$4
	_Step_=$5
	echo ""
	echo ""
	cc 32 "----------------------------------------------------------"
	cc 32 "${_Step_}. 编译【${_Season_Id_}赛季】配置...                                 |"
	cc 32 "----------------------------------------------------------"
	cc 32 "Dir_Root:${_Dir_Root_}  Is_Test:${_Is_Test_}   Season_Id:${_Season_Id_}  Season_Id_Src:${_Season_Id_Src_}"
  # 使用对应赛季的头文件
  cp -rfp ${_Dir_Root_}include_s/s${_Season_Id_}/*.hrl ${_Dir_Root_}include/record/
	# 编译策划配置
	check_dir ${_Dir_Root_}ebin/data/s${_Season_Id_}
	# 是否测试 重置 compile_time
	compile_time ${_Dir_Root_}rebar_config/rebar_data.config ${_Is_Test_}
	# rebar
	chmod a+x ${_Dir_Root_}rebar
	${_Dir_Root_}rebar co -j 8 --config ${_Dir_Root_}rebar_config/rebar_data.config  first_srcdir=data/s${_Season_Id_} first_outdir=ebin/data/s${_Season_Id_}
	check_status $? "填充${_Season_Id_}赛季配置时出错error"

	# 用_Season_Id_Src_赛季的配置填充
	if test "${_Season_Id_}" -ne "${_Season_Id_Src_}" ; then
		cc 35 "cp -fp ${_Dir_Root_}ebin/data/s${_Season_Id_Src_}/ ${_Dir_Root_}ebin/data/s${_Season_Id_}/"
		for _File_ in `ls ${_Dir_Root_}ebin/data/s${_Season_Id_Src_}/`
		do
			_Temp_file_=${_File_%.*}
			if test ! -f ${_Dir_Root_}data/s${_Season_Id_}/${_Temp_file_}.erl; then
			   cp -fp ${_Dir_Root_}ebin/data/s${_Season_Id_Src_}/${_File_} ${_Dir_Root_}ebin/data/s${_Season_Id_}/
			fi
		done
	fi
}

# 获取game.conf中某个配置
game_conf_value()
{
    _Key_=$1
	_Dir_Root_=$2
    if test -n "${_Key_}"; then
        echo `cat ${_Dir_Root_}config/game.config | grep "${_Key_}" | tail -n 1 | sed -e "s/\s*{${_Key_},\s*\(.*\)}.*/\1/"`
    fi
}
