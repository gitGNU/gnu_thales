/* Copyright (C) 2012 Free Software Foundation, Inc.
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
#ifndef DEFINITIONS_H
#define DEFINITIONS_H
#include <guile/2.0/libguile.h>
#include <libguile.h>
#include <libircclient/libircclient.h>
struct scm_t_irc_session
{
  irc_session_t *session;
  SCM event_dispatcher;
  SCM numeric_dispatcher;
  SCM closure;			/* third level of indirection */
};

void scm_throw_from_irc_session (irc_session_t * session);
char* guarded_scm_to_ascii_string (SCM string_or_false);
irc_session_t* scm_to_irc_session(SCM scm_session);

#endif
