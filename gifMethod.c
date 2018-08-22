#include <czmq.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>

#include "jsmn/jsmn.h"
#include "mathlink.h"


// Double check these:

// #define BIND_ADDR_EXT_SIZE 6
#define BIND_ADDR_EXT_SIZE 22
#define UUID_BYTE_SIZE 16
#define UUID_PART_BYTE_SIZE 4
#define TIMESTAMP_SIZE 21
// #define KERNEL_PAIRS_SIZE 113
// #define KERNEL_PAIRS_SIZE 133
#define KERNEL_PAIRS_SIZE 150

#define JSMN_STRICT 1

#define IO_PUB_REPLY 161

#define TO_NUM_PARTS 69

#define TO_PART 12

// launch_kernel

char *launch_kernel(char *path, char *evaluation, size_t timeout);

int mathlink_send_multi_pts(MLINK lp, char* expression, char** output);

int mathlink_send(MLINK lp, char* expression, char** output);

void *ab_malloc(size_t size);

char *str_replace(char *orig, char *rep, char *with);

char *str_replace_repeated(char *orig, char *rep, char *with);

// Generate msd_id

char *getMsgID(void)
{
	unsigned long int *ranMem = (unsigned long int*) calloc(1, sizeof(unsigned long int));

	char *uuidString = (char*) ab_malloc(sizeof(char) * ((UUID_BYTE_SIZE * 2) + 2));

	for(int i = 0; i < 4; i++)
	{
		arc4random_buf(ranMem, UUID_PART_BYTE_SIZE * sizeof(char));
		sprintf(uuidString + ((UUID_BYTE_SIZE / 2) * i) + (i > 0 ? 1 : 0), "%08lx", *ranMem);
		if(i == 0)
		{
			strcat(uuidString, "-");
		};
	};

	uuidString[(UUID_BYTE_SIZE * 2) + 1] = '\0';

	free(ranMem);

	return uuidString;
};

char *getTimestamp(void)
{
	time_t now;
    time(&now);

    char *buff = (char*) ab_malloc(sizeof(char) * TIMESTAMP_SIZE);

    if(!strftime(buff, TIMESTAMP_SIZE * sizeof(char), "%Y-%m-%dT%H:%M:%SZ", gmtime(&now)))
    {
    	return NULL;
    };

    return buff;
};

struct header_element
{
	char *username;
	char *version;
	char *session;
	char *msg_type;
	int len;

	char *in_code;
	// int req_type;
};

struct header_element *structifyHeader(char *mRecvStrGH)
{
	

	int i;

	char old;

	jsmn_parser parser;

	jsmn_init(&parser);

	int tokensNum = jsmn_parse(&parser, mRecvStrGH, strlen(mRecvStrGH), NULL, 265);

	if(tokensNum <= 0)
	{
		return NULL;
	};

	jsmntok_t *tokens2 = (jsmntok_t*) ab_malloc(sizeof(jsmntok_t) * tokensNum);

	jsmn_init(&parser);

	if(jsmn_parse(&parser, mRecvStrGH, strlen(mRecvStrGH), tokens2, tokensNum) <= 0)
	{
		return NULL;
	};

	struct header_element *he = (struct header_element*) calloc(1, sizeof(struct header_element));

	char **toValue = NULL;

	he->len = strlen(mRecvStrGH);

	for(i = 1; i < tokensNum; i += 2)
	{
		old = *(mRecvStrGH + tokens2[i].end);
		*(mRecvStrGH + tokens2[i].end) = '\0';

		if(!strcmp(mRecvStrGH + tokens2[i].start, "username"))
		{
			toValue = &(he->username);
		}
		else if(!strcmp(mRecvStrGH + tokens2[i].start, "version"))
		{
			toValue = &(he->version);
		}
		else if(!strcmp(mRecvStrGH + tokens2[i].start, "session"))
		{
			toValue = &(he->session);
		}
		else if(!strcmp(mRecvStrGH + tokens2[i].start, "msg_type"))
		{
			toValue = &(he->msg_type);
		};

		*(mRecvStrGH + tokens2[i].end) = old;

		// *****************************************************

		if(toValue != NULL && 
			(i + 1) < tokensNum // Just in case
		)
		{
			old = *(mRecvStrGH + tokens2[i + 1].end);
			*(mRecvStrGH + tokens2[i + 1].end) = '\0';

			*toValue = (char*) ab_malloc(sizeof(char) * (strlen(mRecvStrGH + tokens2[i + 1].start) + 1));

			strcpy(*toValue, mRecvStrGH + tokens2[i + 1].start);

			*(mRecvStrGH + tokens2[i + 1].end) = old;

			toValue = NULL;
		};
	};

	free(tokens2);

	

	return he;
};

void addStructifiedContent(struct header_element *he, char *content)
{

	

	int i;

	char old;

	jsmn_parser parser;

	jsmn_init(&parser);

	int tokensNum = jsmn_parse(&parser, content, strlen(content), NULL, 265);

	if(tokensNum <= 0)
	{
		return;
	};

	jsmntok_t *tokens2 = (jsmntok_t*) ab_malloc(sizeof(jsmntok_t) * tokensNum);

	jsmn_init(&parser);

	if(jsmn_parse(&parser, content, strlen(content), tokens2, tokensNum) <= 0)
	{
		return;
	};

	// struct header_element *he = (struct header_element*) calloc(1, sizeof(struct header_element));

	char **toValue = NULL;

	// he->len = strlen(mRecvStrGH);

	for(i = 1; i < tokensNum; i += 2)
	{
		old = *(content + tokens2[i].end);
		*(content + tokens2[i].end) = '\0';

		if(!strcmp(content + tokens2[i].start, "code"))
		{
			toValue = &(he->in_code);
		};

		*(content + tokens2[i].end) = old;

		// *****************************************************

		if(toValue != NULL && 
			(i + 1) < tokensNum // Just in case
		)
		{
			old = *(content + tokens2[i + 1].end);
			*(content + tokens2[i + 1].end) = '\0';

			*toValue = (char*) ab_malloc(sizeof(char) * (strlen(content + tokens2[i + 1].start) + 1));

			strcpy(*toValue, content + tokens2[i + 1].start);

			*(content + tokens2[i + 1].end) = old;

			toValue = NULL;
		};
	};

	free(tokens2);

	
};

char *getHeader(struct header_element *he, char *replyMsgType)
{

	

	char *header;

	char *clientPairs;

	char *kernelPairs = (char*) ab_malloc(sizeof(char) * (strlen(replyMsgType) + KERNEL_PAIRS_SIZE));

	int i;

	char old;

	clientPairs = (char *) calloc(1, sizeof(char) * (he->len + 1));

	if(he->username != NULL)
	{
		strcat(clientPairs, "\"username\":\"");
		strcat(clientPairs, he->username);
		strcat(clientPairs, "\",");
	};
	
	if(he->version != NULL)
	{
		strcat(clientPairs, "\"version\":\"");
		strcat(clientPairs, he->version);
		strcat(clientPairs, "\",");
	};

	if(he->session != NULL)
	{
		strcat(clientPairs, "\"session\":\"");
		strcat(clientPairs, he->session);
		strcat(clientPairs, "\",");
	};

	strcpy(kernelPairs, "\"msg_type\":\"");
	strcat(kernelPairs, replyMsgType);
	strcat(kernelPairs, "\",\"msg_id\":\"");
	strcat(kernelPairs, getMsgID());

	strcat(kernelPairs, "\",\"date\":\"");
	strcat(kernelPairs, getTimestamp());

	strcat(kernelPairs, "\"");

	header = (char*) ab_malloc(sizeof(char) * (strlen(clientPairs) + strlen(kernelPairs) + 4));

	strcpy(header, "{");
	strcat(header, clientPairs);
	strcat(header, kernelPairs);
	strcat(header, "}");

	free(clientPairs);

	free(kernelPairs);

	

	return header;
};

zmsg_t *getReplyMsg(zmsg_t *srcMsg, char *replyMsgType, char *replyContent, int branchOff, struct header_element **heDropoff)
{
	

	if(srcMsg == NULL)
	{
		
		kill(getpid(), SIGKILL);
	};

	

	

	// zmsg_t *srcMsg = zmsg_dup(srcMsgOriginal);

	

	char *frmStr;
	char *sess;
	char *srcHead;

	char *content;

	struct header_element *he;

	zmsg_t *replyMsg;

	zframe_t *frm = zmsg_first(srcMsg);

	for(int i = 0; i < 7; i++)
	{
		

		if(frm == NULL)
		{
			
			kill(getpid(), SIGKILL);
		};

		

		frmStr = zframe_strdup(frm);

		

		if(frmStr == NULL)
		{
			
			kill(getpid(), SIGKILL);
		};

		

		if(i == 0)
		{
			
			sess = strdup(frmStr);
		};
		if(i == 3)
		{
			
			srcHead = strdup(frmStr);
			
			he = structifyHeader(srcHead);
			
		};
		if(i == 6)
		{
			
			content = strdup(frmStr);
			addStructifiedContent(he, content);
		};

		free(frmStr);

		

		frm = zmsg_next(srcMsg);

		
	};

	zmsg_first(srcMsg);

	// zmsg_destroy(&srcMsg);

	
	
	
	
	

	replyMsg = zmsg_new();

	zmsg_addstr(replyMsg, sess);

	zmsg_addstr(replyMsg, "<IDS|MSG>");
	zmsg_addstr(replyMsg, "");

	zmsg_addstr(replyMsg, getHeader(he, replyMsgType));

	if(!branchOff)
	{
		zmsg_addstr(replyMsg, srcHead);
	}
	else
	{
		zmsg_addstr(replyMsg, "{}");
	};
	
	zmsg_addstr(replyMsg, "{}");
	
	zmsg_addstr(replyMsg, replyContent);

	if(heDropoff != NULL)
	{
		*heDropoff = he;
	};

	

	return replyMsg;
};

int main(int argc, char	**argv) 
{
	if(argc < 2)
	{
		return -1;
	};

	FILE *fd = fopen(argv[1], "r");

	if(fd == NULL)
	{
		return -1;
	};

	char* buff = NULL;
	size_t len;
	size_t bytes_read = getdelim(&buff, &len, '\0', fd);

	if(bytes_read == -1)
	{
		return -1;
	};

	int tokensNum;

	jsmn_parser parser;
	jsmn_init(&parser);

	tokensNum = jsmn_parse(&parser, buff, strlen(buff), NULL, 265);

	if(tokensNum <= 0)
	{
		return -1;
	};

	jsmntok_t *tokens = (jsmntok_t*) ab_malloc(sizeof(jsmntok_t) * tokensNum);

	jsmn_init(&parser);

	if(jsmn_parse(&parser, buff, strlen(buff), tokens, tokensNum) <= 0)
	{
		return -1;
	};

	char *tcp = "tcp://";

	char *hbString = (char*) ab_malloc(sizeof(char) * (strlen(tcp) + BIND_ADDR_EXT_SIZE));
	strcpy(hbString, tcp);
	char *ipString = (char*) ab_malloc(sizeof(char) * (strlen(tcp) + BIND_ADDR_EXT_SIZE));
	strcpy(ipString, tcp);
	char *cString = (char*) ab_malloc(sizeof(char) * (strlen(tcp) + BIND_ADDR_EXT_SIZE));
	strcpy(cString, tcp);
	char *iString = (char*) ab_malloc(sizeof(char) * (strlen(tcp) + BIND_ADDR_EXT_SIZE));
	strcpy(iString, tcp);
	char *sString = (char*) ab_malloc(sizeof(char) * (strlen(tcp) + BIND_ADDR_EXT_SIZE));
	strcpy(sString, tcp);

	char old;
	char *bindWhich;

	// START: May not be needed

	int i;

	for(i = 1; i < tokensNum; i++)
	{
		old = *(buff + tokens[i].end);
		*(buff + tokens[i].end) = '\0';


		if(!strcmp(buff + tokens[i].start, "ip"))
		{
			if(
				(i + 1) < tokensNum // Just in case
			)
			{
				old = *(buff + tokens[i + 1].end);
				*(buff + tokens[i + 1].end) = '\0';

				strcat(hbString, buff + tokens[i + 1].start);
				strcat(hbString, ":");

				strcat(ipString, buff + tokens[i + 1].start);
				strcat(ipString, ":");

				strcat(cString, buff + tokens[i + 1].start);
				strcat(cString, ":");

				strcat(iString, buff + tokens[i + 1].start);
				strcat(iString, ":");

				strcat(sString, buff + tokens[i + 1].start);
				strcat(sString, ":");

				*(buff + tokens[i + 1].end) = old;
			};
		};

		*(buff + tokens[i].end) = old;
	};

	// END: May not be needed

	for(i = 1; i < tokensNum; i++)
	{
		old = *(buff + tokens[i].end);
		*(buff + tokens[i].end) = '\0';

		bindWhich = NULL;

		if(!strcmp(buff + tokens[i].start, "stdin_port"))
		{
			bindWhich = iString;
		}
		else if(!strcmp(buff + tokens[i].start, "control_port"))
		{
			bindWhich = cString;
		}
		else if(!strcmp(buff + tokens[i].start, "hb_port"))
		{
			bindWhich = hbString;
		}
		else if(!strcmp(buff + tokens[i].start, "shell_port"))
		{
			bindWhich = sString;
		}
		else if(!strcmp(buff + tokens[i].start, "iopub_port"))
		{
			bindWhich = ipString;
		};

		*(buff + tokens[i].end) = old;

		// *****************************************************

		if(bindWhich != NULL && 
			(i + 1) < tokensNum // Just in case
		)
		{
			old = *(buff + tokens[i + 1].end);
			*(buff + tokens[i + 1].end) = '\0';

			strcat(bindWhich, buff + tokens[i + 1].start);

			*(buff + tokens[i + 1].end) = old;
		};
	};

	free(tokens);

	pid_t fpid = fork();

	if(!fpid)
	{
		// Creating Heartbeat socket on arbitrary port on its own thread
		CZMQ_EXPORT zsock_t *heartbeatSocket  = zsock_new_rep(hbString);

		if(heartbeatSocket == NULL)
		{
			return -1;
		};

		while(1)
		{

			zmsg_t *ping; // We recreate message each time as "header" nullifies it
	    
			ping = zmsg_recv(heartbeatSocket); // Client asks if kernel is still running

			if(ping == NULL)
			{
				continue;
			};

			zmsg_send(&ping, heartbeatSocket); // Answer immediately
		};
	};

	// Creating I/O Pub socket on arbitrary port
	CZMQ_EXPORT zsock_t *ioPubSocket  = zsock_new_pub(ipString);

	// Creating Control socket on arbitrary port
	CZMQ_EXPORT zsock_t *controlSocket = zsock_new_router(cString);

	// Creating Stdin socket on arbitrary port
	CZMQ_EXPORT zsock_t *inputSocket = zsock_new_router(iString);

	// Creating Shell socket on arbitrary port
	CZMQ_EXPORT zsock_t *shellSocket = zsock_new_router(sString);

	// int waitTime = -1;
	// zmq_setsockopt(shellSocket, ZMQ_RCVTIMEO, &waitTime, sizeof(int));

	CZMQ_EXPORT zpoller_t *poller = zpoller_new(shellSocket, ioPubSocket, controlSocket, inputSocket, NULL);

	CZMQ_EXPORT zpoller_t *pollers = zpoller_new(shellSocket, NULL);

	zsock_t *which;

	char *recvStr = NULL;

	char *sess;

	char *mRecvStr;

	char *ioHead;

	int plac;

	CZMQ_EXPORT zmsg_t *srcMsg;

	CZMQ_EXPORT zmsg_t *statMsgBusy;
	CZMQ_EXPORT zmsg_t *statMsgBusyDup;
	CZMQ_EXPORT zmsg_t *statMsgIdle;

	CZMQ_EXPORT zmsg_t *shellReply;

	CZMQ_EXPORT zmsg_t *ioPubReply = NULL;

	struct header_element *he;
	struct header_element **heDropoff = (struct header_element **) ab_malloc(sizeof(struct header_element *));

	// Parts of reply strings
	char *replyMsgType;
	char *replyContent;
	char *ioPubReplyContent = NULL;

	// Computation strings
	char *toRes = NULL;
	char *toDoCloud = NULL;

	// Result strings
	char *amPNG;
	char *amPNGError;
	// char *amPNGWidth;
	// char *amPNGHeight;
	char *amDoText;
	char *amDoCloud;

	// Pointer for (un)escaped strings
	char *espStr;

	// Space for string of current input count
	char *countStr = (char*) ab_malloc(sizeof(char) * ((sizeof(int) * 8) + 1 + 1));

	// Bools
	int doCloud = 0;
	int doText = 0;
	int count = 1;

	

 	// Allocate space to recieve kernel output
	char **output = (char **) ab_malloc(sizeof(char*));
	*output = NULL;

	//START: Start kernel

		MLINK lp = NULL;
 		MLENV env;

		// int ret_code;
		int error = 0;

		// Build command line used to launch wstp connection.
		char* cmdline[3] = {"-linklaunch", "-linkname", "/Applications/Mathematica.app/Contents/MacOS/WolframKernel"};

		// Initialize mathlink.
		env = MLInitialize(NULL);
		if (env == NULL) {
			return -1;
		};

		// MLOpen Kernel with arguments.
		lp = MLOpenArgcArgv(env, 3, cmdline, &error);
		if(lp == NULL || error != 0) {
			return -1;
		};

		// Activate link.
		if (!MLActivate(lp)) {
			return -1;
		};

	// END: Start Kernel

	// Check whether Wolfram output is composed of basic enough objects so that Jupyter output can be plaintext
	mathlink_send(lp, "JupyterKernel`Private`doText[expr_]:=Module[{pObjects},pObjects=KeyDrop[GroupBy[DeleteCases[Cases[expr,_?AtomQ,{0,Infinity},Heads->True],List|Association],Head],{Integer,String,Real}];If[Keys[pObjects]==={Symbol},Return[AllTrue[pObjects[Symbol],(ToString[Definition[#1]]===\"Null\")&]];,Return[Length[pObjects]==0];]];", output);
	free(*output);

	// Evaluate input, and return an Association of the result and any messages generated during the input's evaluation
	mathlink_send(lp, "JupyterKernel`Private`jupEval[expr_] := Module[{$oldMessages, stream, msgs, eval},  eval = Association[];    $oldMessages = $Messages;  stream = OpenWrite[];  $Messages = Append[$Messages, stream];    AssociateTo[eval, \"res\" -> expr];    msgs = Import[stream[[1]], \"String\"];    $Messages = $oldMessages;  Close[stream];    AssociateTo[eval, \"msgs\" -> msgs];  Return[eval];  ]; SetAttributes[JupyterKernel`Private`jupEval, HoldAll];", output);
	free(*output);

	// Define Interact wrapper for cloud deploy checking; also redirect removal of wrapper to jupEval above
	mathlink_send(lp, "InteractQ[expr_] := MatchQ[expr, Hold[Interact[___]]]; Uninteract[Interact[expr___]] ^:= JupyterKernel`Private`jupEval[expr]; SetAttributes[Interact, HoldAll]; Uninteract[expr___] := JupyterKernel`Private`jupEval[expr]; SetAttributes[Uninteract, HoldAll];", output);
	free(*output);

	while(1)
	{

		

		which = (zsock_t *) zpoller_wait(poller, -1);

		if(which == shellSocket)
		{
			
			while(which != NULL)
			{
				srcMsg = zmsg_recv(shellSocket);

				which = (zsock_t *) zpoller_wait(pollers, 250);
			};

			getReplyMsg(srcMsg, "", "{}", 0, heDropoff);
			he = (*heDropoff);

			if(!strcmp(he->msg_type, "kernel_info_request"))
			{
				// Reply with information
				replyMsgType = "kernel_info_reply";
				replyContent = "{\"protocol_version\": \"5.3.0\",\"implementation\": \"WL\",\"implementation_version\": \"0.0.1\",\"language_info\": {\"name\": \"Wolfram Language\",\"version\": \"11.3.0\",\"mimetype\": \"text/plain\",\"file_extension\": \".m\",\"pygments_lexer\": \"python\",\"codemirror_mode\": \"python\"},\"banner\" : \"Mathematica Copyright 2018\"}";
				replyContent = "{\"protocol_version\": \"5.3.0\",\"implementation\": \"WL\"}";
			}
			else if(!strcmp(he->msg_type, "is_complete_request"))
			{
				// For the moment, just replies "unknown"; maybe add SyntaxQ checking?
				replyMsgType = "is_complete_reply";
				replyContent = "{\"status\":\"unknown\"}";
			}
			else if(!strcmp(he->msg_type, "execute_request"))
			{
				// Execute code, and reply on shell with notification of completed execution, and send output on IO Pub
				replyMsgType = "execute_reply";

				// Generate string of input count
				sprintf(countStr, "%d", count);
				count++;

				// Space for notification of completed execution
				replyContent = (char*) ab_malloc(sizeof(char) * ((sizeof(int) * 8) + 1 + 1 + 70));

				strcpy(replyContent, "{\"status\":\"ok\",\"execution_count\":");
				strcat(replyContent, countStr);
				strcat(replyContent, ",\"user_expressions\":{}}");

				// Commented out with introduction of ToExpression
				// espStr = str_replace(he->in_code, "\\\"", "\"");

				// START: Compute Result

					// Space for computing result of input
					toRes = (char*) ab_malloc(sizeof(char) * (strlen(he->in_code) + 50 + 292 + 1));

					strcpy(toRes, "JupyterKernel`Private`$jupResEval = ToExpression[\""); // 50 chars
					strcat(toRes, he->in_code);
					// Catch any messages, and store them in JupyterKernel`Private`$msgs
					strcat(toRes, "\", InputForm, Uninteract]; JupyterKernel`Private`$res = JupyterKernel`Private`$jupResEval[\"res\"]; JupyterKernel`Private`$msgs = JupyterKernel`Private`$jupResEval[\"msgs\"]; If[FailureQ[JupyterKernel`Private`$jupResEval], JupyterKernel`Private`$res = $Failed; JupyterKernel`Private`$msgs = {};];"); // 292 chars

					mathlink_send(lp, toRes, output);
					free(*output);

					free(toRes);

				// END: Compute Result

				// START: Check Cloud Deploy

					// Space for checking whether to deploy to cloud
					toDoCloud = (char*) ab_malloc(sizeof(char) * (strlen(he->in_code) + 30 + 40 + 1));

					// Check for Interact wrapper from user telling us to use cloud
					strcpy(toDoCloud, "TrueQ[InteractQ[ToExpression[\""); // 30 chars
					strcat(toDoCloud, he->in_code);
					// Do not deploy to cloud if no connection to said cloud
					strcat(toDoCloud, "\", InputForm, Hold]]] && $CloudConnected"); // 40 chars

					mathlink_send(lp, toDoCloud, output);
					amDoCloud = *output;

					

					free(toDoCloud);

					doCloud = 0;
					if(strlen(amDoCloud) > 0)
						//Just in case
					{
						doCloud = (amDoCloud[0] == 'T');
					};

					free(amDoCloud);

				// END: Check Cloud Deploy

				// free(espStr);

				if(doCloud)
				{
					// Generate inline, image encoding of any errors encountered during the evaluation of the input
					mathlink_send(lp, "BaseEncode[ExportByteArray[Rasterize[Style[JupyterKernel`Private`$msgs, Darker[Red]]], \"PNG\"]]", output);
					amPNGError = *output;

					// This is here to compute how much to constrain the cloud window
					// However, since this kernel does not know if the browser is logged into Wolfram Cloud, a minimum size is required for the login window
					// So, at the moment, a size is computed, but is not used for constraining the iframe
					mathlink_send(lp, "JupyterKernel`Private`$resID = ImageDimensions[Rasterize[JupyterKernel`Private`$res]] + 25;", output);
					free(*output);

					// Deploy output to cloud, and return html code to embed cloud version of output in browser
					// Commenting out window constraining ImageSize option
					// ", ImageSize -> JupyterKernel`Private`$resID"
					mathlink_send(lp, "EmbedCode[CloudDeploy[JupyterKernel`Private`$res], \"HTML\"][[1]][\"CodeSection\"][\"Content\"]", output);
					amPNG = *output;

					// Escape generated html
					espStr = str_replace(amPNG, "\"", "\\\"");
					free(amPNG);

					// Generate html Jupyter output message for IO Pub

					ioPubReplyContent = (char*) ab_malloc(sizeof(char) * (strlen(espStr) + strlen(amPNGError) + strlen(countStr) + 19 + 69 + 3 + 24 + 1));

					strcpy(ioPubReplyContent, "{\"execution_count\":"); // 19 chars
					strcat(ioPubReplyContent, countStr);
					strcat(ioPubReplyContent, ",\"data\":{\"text/html\":\"<div><img alt=\\\"\\\" src=\\\"data:image/png;base64,"); // 69 chars

					strcat(ioPubReplyContent, amPNGError);
					free(amPNGError);

					strcat(ioPubReplyContent, "\\\">"); // 3 chars
					strcat(ioPubReplyContent, espStr);
					strcat(ioPubReplyContent, "</div>\"}, \"metadata\":{}}"); // 24 chars

					free(espStr);
				}
				else
				{
					// START: Format Output as Text or Image?

						// If output only includes a select few, definitely text objects, and no errors were generated, return output as text 
						mathlink_send(lp, "JupyterKernel`Private`doText[JupyterKernel`Private`$res] && StringLength[JupyterKernel`Private`$msgs] == 0", output);
						amDoText = *output;

						doText = 0;
						if(strlen(amDoText) > 0)
							//Just in case
						{
							doText = (amDoText[0] == 'T');
						};
						free(amDoText);

					// END: Format Output as Text or Image?

					if(doText)
					{
						// Get input result as text
						mathlink_send(lp, "JupyterKernel`Private`$res", output);
						amPNG = *output;

						// Escape generated text
						espStr = str_replace(amPNG, "\"", "\\\"");
						free(amPNG);
						amPNG = espStr;
						espStr = str_replace(amPNG, "\n", "\\n");
						free(amPNG);

						// Generate plaintext Jupyter output message for IO Pub

						ioPubReplyContent = (char*) ab_malloc(sizeof(char) * (strlen(countStr) + strlen(amPNG) + 19 + 23 + 18 + 1));

						strcpy(ioPubReplyContent, "{\"execution_count\":"); // 19 chars
						strcat(ioPubReplyContent, countStr);
						strcat(ioPubReplyContent, ",\"data\":{\"text/plain\":\""); // 23 chars
						strcat(ioPubReplyContent, espStr);
						strcat(ioPubReplyContent, "\"}, \"metadata\":{}}"); // 18 chars

						free(espStr);
					}
					else
					{
						mathlink_send(lp, "If[Head[JupyterKernel`Private`$res] === Manipulate, JupyterKernel`Private`MaybeRasterize[expr_] := expr;, JupyterKernel`Private`MaybeRasterize[expr_] := Rasterize[expr];];", output);
						free(*output);

						// Generate inline, image encoding of any errors encountered during the evaluation of the input
						mathlink_send(lp, "If[StringLength[JupyterKernel`Private`$msgs] == 0, "", BaseEncode[ExportByteArray[Rasterize[Style[JupyterKernel`Private`$msgs, Darker[Red]]], \"PNG\"]]]", output);
						amPNGError = *output;

						// Generate inline, image encoding of an Export of the Wolfram output
						mathlink_send(lp, "BaseEncode[ExportByteArray[JupyterKernel`Private`MaybeRasterize[JupyterKernel`Private`$res], \"PNG\"]]", output);
						amPNG = *output;

						// // Get dimensions of said image
						// mathlink_send(lp, "JupyterKernel`Private`$resID = ImageDimensions[Rasterize[JupyterKernel`Private`$res]];", output);
						// free(*output);

						// // Get width of said image
						// mathlink_send(lp, "JupyterKernel`Private`$resID[[1]]", output);
						// amPNGWidth = *output;

						// // Get height of said image
						// mathlink_send(lp, "JupyterKernel`Private`$resID[[2]]", output);
						// amPNGHeight = *output;

						// Start: Generate Image Jupyter Output Message (for IO Pub)

						ioPubReplyContent = (char*) ab_malloc(sizeof(char) * (strlen(countStr) + strlen(amPNGError) + strlen(amPNG) + 19 + 69 + 51 + 27 + 1));

						strcpy(ioPubReplyContent, "{\"execution_count\":"); // 19 chars
						strcat(ioPubReplyContent, countStr);
						strcat(ioPubReplyContent, ",\"data\":{\"text/html\":\"<div><img alt=\\\"\\\" src=\\\"data:image/png;base64,"); // 69 chars

						strcat(ioPubReplyContent, amPNGError);
						free(amPNGError);

						// strcat(ioPubReplyContent, "\\\" height=\\\""); // [recheck] chars

						// strcat(ioPubReplyContent, amPNGWidth);
						// free(amPNGWidth);

						// strcat(ioPubReplyContent, "\\\" width=\\\""); // [recheck] chars

						// strcat(ioPubReplyContent, amPNGHeight);
						// free(amPNGHeight);

						strcat(ioPubReplyContent, "\\\"><img alt=\\\"Output\\\" src=\\\"data:image/png;base64,"); // 51 chars

						strcat(ioPubReplyContent, amPNG);
						free(amPNG);

						strcat(ioPubReplyContent, "\\\"></div>\"}, \"metadata\":{}}"); // 27 chars
					};
				};

				

				ioPubReply = getReplyMsg(srcMsg, "execute_result", ioPubReplyContent, 0, NULL);

				
			}
			else
			{
				continue;
				// kill(getpid(), SIGKILL);
			};

			// START: Announce that the kernel is busy

			statMsgBusy = getReplyMsg(srcMsg, "status", "{\"execution_state\":\"busy\"}", 
							//Branch off
							1, NULL
						);
			statMsgBusyDup = zmsg_dup(statMsgBusy);
			zmsg_send(&statMsgBusy, ioPubSocket);
			zmsg_destroy(&statMsgBusy);

			

			// END: Announce that the kernel is busy

			// START: Reply to _request

			shellReply = getReplyMsg(srcMsg, replyMsgType, replyContent, 0, NULL);
			zmsg_send(&shellReply, shellSocket);
			zmsg_destroy(&shellReply);

			

			if(ioPubReply != NULL)
			{
				zmsg_send(&ioPubReply, ioPubSocket);
				zmsg_destroy(&ioPubReply);

				free(ioPubReplyContent);

				
			};
			ioPubReply = NULL;

			// END: Reply to _request

			// START: Announce that the kernel is idle

			statMsgIdle = getReplyMsg(statMsgBusyDup, "status", "{\"execution_state\":\"idle\"}", 0, NULL);
			zmsg_send(&statMsgIdle, ioPubSocket);
			zmsg_destroy(&statMsgIdle);
			zmsg_destroy(&statMsgBusyDup);

			

			// END: Announce that the kernel is idle

			
		}
		else
		{
			
			// kill(getpid(), SIGKILL);
		};
	};

  return 0;
};

// /*			launch_kernel
// Purpose: Launches a kernel and executes script

// Arguments:
// 	flagsv : flagsv containing environment so far.
// 	evaluation : the wolfram language code to be executed in the kernel  
// 	timeout : timeout to be used.
// */
// char *launch_kernel(char *path, char *evaluation, size_t timeout){
// 	

// 	MLINK lp = NULL;
//  	MLENV env;

// 	char **output = (char **) ab_malloc(sizeof(char*));
// 	char *res_out = strdup("0");

// 	


// 	*output = NULL;
// 	int ret_code;
// 	int error = 0;

// 	//Build command line used to launch wstp connection.
// 	char* cmdline[3] = {"-linklaunch", "-linkname", "/Applications/Mathematica.app/Contents/MacOS/WolframKernel"};

// 	

// 	//Initialize mathlink.
// 	env = MLInitialize(NULL);
// 	if (env == NULL) {
// 		
// 		return res_out;
// 	};

// 	

// 	

// 	//MLOpen Kernel with arguments.
// 	lp = MLOpenArgcArgv(env, 3, cmdline, &error);
// 	
// 	if(lp == NULL || error != 0) {
// 		
// 		return res_out;
// 	};

// 	

// 	//Activate link.
// 	if (!MLActivate(lp)) {
// 		return res_out;
// 	};

// 	

// 	ret_code = mathlink_send(lp, evaluation, output);

// 	

// 	if(*output != NULL)
// 	{
// 		free(res_out);

// 		res_out = strdup(*output);
// 		free(output);
// 	};

// 	if(lp != NULL)
// 	{
// 		MLClose(lp);
// 	};

// 	if(env != NULL)
// 	{
// 		MLDeinitialize(env);
// 	};
// 	////

// 	
// 	return res_out;
// };

int mathlink_send_multi_pts(MLINK lp, char* expression, char** output){

	// // Get number of parts

	// char *toNumPts = (char*) ab_malloc(sizeof(char) * (TO_NUM_PARTS + strlen(expression)));

	// strcpy(toNumPts, "JupyterKernel`Private`$res = ");
	// strcat(toNumPts, expression);
	// strcat(toNumPts, "; JupyterKernel`Private`$resPts = StringPartition[JupyterKernel`Private`$res, UpTo[1500]]; Length[JupyterKernel`Private`$resPts]");

	// mathlink_send(lp, toNumPts, output);

	// int numPts = atoi(*output);
	// int numPtsLength = strlen(*output);

	

	// free(*output);

	// // Put parts together

	// char *allPts = (char*) calloc(1, sizeof(char) * ((numPts * 1500) + 1));

	// char *toPt = (char*) ab_malloc(sizeof(char) * (TO_PART + numPtsLength));
	// char *ptIndex = (char*) ab_malloc(sizeof(char) * ((sizeof(int) * 8) + 1 + 1));

	// for(int i = 1; i <= numPts; i++)
	// {
	// 	strcpy(toPt, "JupyterKernel`Private`$resPts[[");

	// 	// Resolve itoa to ANSI solution
	// 	sprintf(ptIndex, "%d", i);
	// 	strcat(toPt, ptIndex);

	// 	strcat(toPt, "]]");

	// 	

	// 	// strcat allPts

	// 	mathlink_send(lp, toPt, output);

	// 	

	// 	strcat(allPts, *output);

	// 	free(*output);
	// };

	

	// // abort();

	// char *allPts = (char*) calloc(1, sizeof(char) * 760);

	// strcpy(allPts, "iVBORw0KGgoAAAANSUhEUgAAAAgAAAARCAIAAABvtHqPAAAAhXpUWHRSYXcgcHJvZmlsZSB0eXBlIGV4aWYAAHjaVYvBDcMwDAP/mqIjyLJM2uMYiQNkg45fuu0n9yApgbT1vi97felp2dgxABc5csRU6P6juNciDXn+X9MfZGWgoRhTluEkhnJXkqKFN+LCAahYcIbnIV8gNQN3o86928QyPusLVffpbh/5eCey76LuBgAAAAlwSFlzAAALEwAACxMBAJqcGAAAADx0RVh0U29mdHdhcmUAQ3JlYXRlZCB3aXRoIHRoZSBXb2xmcmFtIExhbmd1YWdlIDogd3d3LndvbGZyYW0uY29tXKKmhQAAACF0RVh0Q3JlYXRpb24gVGltZQAyMDE4OjA3OjE5IDIwOjU3OjA3jAg5CwAAAOFJREFUGJWtkCFPw1AUhb+3sABiJGBQ/AUE/+BhEXUYphskOGbrEIihipqhaoSkqpiaYWaemRmiIeiKNnmIQioOog1qDq4798vJOTlGEptusPH7B/D1+nBtjDHm9P7lrSeSXByAnWWLLImAaFFKQm0RwiQvJUlyU4udNRLyS4t1TbtO42RZ+nUMUSkNGA5hj2p1E1xehM8V2/D0XvfhnoOTu3yePZ4f8g1nR/ugZjWGW+e7jCIZcxx7CUn5BLhyVeOL1EI4L9QBtR/ToG9vo7Tzmt8RP+uard3RaKeT5v/W/QGZqnyrQ1F8DwAAAABJRU5ErkJggg==");

	*output = strdup("error");

	return 0;
};

/************************************************
				mathlink_send
*************************************************
Arguments: MLINK lp : the input link already launched
	string expression : the wolfram expression to be sent and evaluated
	int mode : selects MLPutFunction 0 is default. (1 and 2) set up piped input
*************************************************
Purpose: Sends a wolfram expression through mathlink lp.
*************************************************/
int mathlink_send(MLINK lp, char* expression, char** output){

	// char *expressionNew = (char*) ab_malloc(sizeof(char) * (strlen(expression) + ));

	const unsigned char* retval = NULL;
	char *buff = NULL;
	int length;

	
	//Start an evaluation packet
	MLPutFunction(lp, "EvaluatePacket", 1);
	MLPutFunction(lp, "ToString", 1);
	
	//setting stdin or sending an expression to evaluate
	MLPutFunction(lp, "ToExpression", 1);

		
	//Put the expression
	size_t chunk_size = 50000000;
	char* part = (char*) ab_malloc((chunk_size + 1) * sizeof(char));
	if(strlen(expression) > chunk_size)
	{
		MLPutFunction(lp, "StringJoin", (strlen(expression)+chunk_size-1)/chunk_size);
		for(size_t i = 0; i*chunk_size < strlen(expression); i += 1)
		{
			strncpy(part, expression + i*chunk_size, chunk_size);
			part[chunk_size] = '\0';
			MLPutByteString(lp, (unsigned char*) part, (long)strlen(part));
		}
	}
	else
	{
		MLPutByteString(lp, (unsigned char*)expression, (long)strlen(expression));
	};

	free(part);

	
	//End the packet and send it
	MLEndPacket(lp);
	MLFlush(lp);

	int type = 0;
	int error_out = 10;
	int error = MLEOK;
	int ret_code = 0;
	int data_type = 0;
	char *p1;
	int display_next_message = 1;
 	char *exitcode_text = "WolframScript`Private`Output::ExitCode: ";

 	
	while(type != 3 && error == MLEOK)
	{
		type=MLNextPacket(lp);
		while(type == 0 && error == MLEOK){
			MLNewPacket(lp);
			error = MLError(lp);
		}
	
		if(error == MLEOK && type == 2)
		{
		 	//Get return value, place it in a string
			MLGetByteString(lp, &retval, &length, 0);
			if(buff != NULL)
			{
				free(buff);
			};
		 	buff = strdup((const char*) retval);
		 	
		 	//Release its allocated memory	
		 	if(retval != NULL)
		 	{
			 	MLReleaseByteString(lp, retval, length);
			 	retval = NULL;
		 	};
		 	p1 = strstr(buff, exitcode_text);
			if(p1 != NULL)
			{
				buff = p1+strlen(exitcode_text);
				if(strlen(buff) > 0)
				{
					ret_code = atoi(buff);
				}
	 		}
	 	}
		display_next_message = 0;
	 	if(type != 0 && type != 2 && type != 3)
	 	{
	 		
	 		MLNewPacket(lp);
	 		if(type == 5){display_next_message = 1;}
	 	}
	}

	if(error == 0 && (type == RETURNPKT || type == 0))
	{
	 	//Get return value, place it in a string
		MLGetByteString(lp, &retval, &length, 0);
		if(buff != NULL)
		{
			free(buff);
		};
	 	buff = strdup((const char*) retval);
	 	
	 	//Release its allocated memory	
	 	if(retval != NULL)
	 	{
		 	MLReleaseByteString(lp, retval, length);
		 	retval = NULL;
	 	};
	 	
	 	
	}
	else
	{
		buff = "";
	 	MLClearError(lp);
	};

		
	*output = buff;

	
	
	return ret_code;
	return 0;
};

// Just in case of a ab_malloc fail
void *ab_malloc(size_t size)
{
	void *mem = malloc(size);
	if(mem == NULL)
	{
		kill(getpid(), SIGKILL);
	};

	return mem;
};

// Open code; @jmucchiello
// You must free the result if result is non-NULL.
char *str_replace(char *orig, char *rep, char *with) {
    char *result; // the return string
    char *ins;    // the next insert point
    char *tmp;    // varies
    int len_rep;  // length of rep (the string to remove)
    int len_with; // length of with (the string to replace rep with)
    int len_front; // distance between rep and end of last rep
    int count;    // number of replacements

    // sanity checks and initialization
    if (!orig || !rep)
    	kill(getpid(), SIGKILL);
        // return NULL;
    len_rep = strlen(rep);
    if (len_rep == 0)
    	kill(getpid(), SIGKILL);
        // return NULL; // empty rep causes infinite loop during count
    if (!with)
        with = "";
    len_with = strlen(with);

    // count the number of replacements needed
    ins = orig;
    for (count = 0; (tmp = strstr(ins, rep)); ++count) {
        ins = tmp + len_rep;
    }

    tmp = result = ab_malloc(strlen(orig) + (len_with - len_rep) * count + 1);

    if (!result)
        return NULL;

    // first time through the loop, all the variable are set correctly
    // from here on,
    //    tmp points to the end of the result string
    //    ins points to the next occurrence of rep in orig
    //    orig points to the remainder of orig after "end of rep"
    while (count--) {
        ins = strstr(orig, rep);
        len_front = ins - orig;
        tmp = strncpy(tmp, orig, len_front) + len_front;
        tmp = strcpy(tmp, with) + len_with;
        orig += len_front + len_rep; // move to next "end of rep"
    }
    strcpy(tmp, orig);
    return result;
};

char *str_replace_repeated(char *orig, char *rep, char *with) {
	char *res;
	char *repStr = str_replace(orig, rep, with);
	if(strcmp(orig, rep) == 0)
	{
		return repStr;
	}
	else
	{
		res = str_replace_repeated(repStr, rep, with);
		free(repStr);
		return res;
	};
};