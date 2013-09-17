/* Copyright (C) 2012, 2013 Free Software Foundation, Inc.
 *
 * This file is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */

#include "definitions.h"

SCM_DEFINE(scm_irc_cmd_join, "irc-cmd-join", 3, 0, 0,
           (SCM scm_session, SCM scm_channel, SCM scm_reason),
           "Join channel")
{
  scm_dynwind_begin(0);
  irc_session_t *session = scm_to_irc_session(scm_session);
  char *channel = guarded_scm_to_ascii_string(scm_channel);
  char *reason = guarded_scm_to_ascii_string(scm_reason);

  irc_cmd_join(session, channel, reason);
  scm_dynwind_end();
  return SCM_UNSPECIFIED;
}


SCM_DEFINE(scm_irc_cmd_quit, "irc-cmd-quit", 2, 0, 0,
           (SCM s, SCM scm_reason),
           "Polite disconnect")
{
  irc_session_t *session = scm_to_irc_session(s);
  char *reason = guarded_scm_to_ascii_string(scm_reason);
  int error = irc_cmd_quit(session, reason);
  if (error)
    scm_throw_from_irc_session(session);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (scm_irc_cmd_part, "irc-cmd-part", 2, 0, 0,
	    (SCM s, SCM scm_channel),
	    "Leaves the IRC channel. This function is used to leave the IRC channel"
	    "you’ve already joined to. An attempt to leave the channel you aren’t"
	    "in results a LIBIRC_RFC_ERR_NOTONCHANNEL server error.")
{
  irc_session_t *session = scm_to_irc_session(s);
  char *channel = guarded_scm_to_ascii_string (scm_channel);
  int error = irc_cmd_part (session, channel);
  if (error)
    scm_throw_from_irc_session(session);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(scm_irc_cmd_names, "irc-cmd-names", 2, 0, 0,
           (SCM s, SCM scm_channel),
           "List users on specified channel")
{
  irc_session_t *session = scm_to_irc_session(s);
  char *channel = guarded_scm_to_ascii_string(scm_channel);
  int error = irc_cmd_names(session, channel);
  if (error)
    scm_throw_from_irc_session(session);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(scm_irc_cmd_whois, "irc-cmd-whois", 2, 0, 0,
           (SCM s, SCM scm_nick),
           "Query about nick.")
{
  irc_session_t *session = scm_to_irc_session(s);
  char *nick = guarded_scm_to_ascii_string(scm_nick);
  int error = irc_cmd_whois(session, nick);
  if (error)
    scm_throw_from_irc_session(session);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(scm_irc_send_raw, "irc-send-raw", 2, 0, 0,
           (SCM s, SCM scm_cmd),
           "Raw command to pass to server.")
{
  irc_session_t *session = scm_to_irc_session(s);
  char *cmd = guarded_scm_to_ascii_string(scm_cmd);
  irc_send_raw(session, "%s", cmd);
  return SCM_UNSPECIFIED;
}
